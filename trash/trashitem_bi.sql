create or replace
TRIGGER "BS"."TRASHITEMID_BI" 
 BEFORE 
 INSERT
 ON bs.TRASHITEM
 REFERENCING OLD AS OLD NEW AS NEW
 FOR EACH ROW 
DECLARE
  opertp    NUMBER;
  newamount NUMBER;
  iscorr    NUMBER;
  l_old_curr_balance  NUMBER;
  l_cust_balance      NUMBER;
  l_cust_old_balance  NUMBER;
  l_cust_tech_balance NUMBER;
  l_is_payment BOOLEAN;
  l_old_amount NUMBER;
  l_new_amount NUMBER;
BEGIN

IF :NEW.trashitemid IS NULL
THEN

  SELECT tr.opergroupid, tr.iscorrection
    INTO opertp, iscorr
    FROM trashoperation tr
  WHERE operationid = :NEW.operationid;

  SELECT trashitem_sq.NEXTVAL INTO :NEW.trashitemid FROM dual;

  :NEW.trashitemnumber := ltrim(rtrim(:NEW.trashitemnumber));
  :NEW.amount          := round(nvl(:NEW.amount, 0), 2);

  IF :NEW.custkey IS NOT NULL THEN

    IF opertp = 2 AND iscorr = 0 THEN
      -- gadaxda
      newamount := - ROUND(:NEW.amount, 2);
    ELSE
      newamount := + ROUND(:NEW.amount, 2);
    END IF;

    IF :NEW.operationid != 208 THEN

      SELECT
        ROUND(NVL(balance, 0), 2), ROUND(NVL(old_balance, 0), 2),
        ROUND(NVL(tech_balance, 0), 2), ROUND(NVL(curr_balance, 0), 2)
      INTO
        :NEW.balance, :NEW.old_balance,
        :NEW.tech_balance, l_old_curr_balance
      FROM
        trashcustomer
      WHERE custkey = :NEW.custkey;

      -- payment (use normilized current balance)
      l_is_payment := :NEW.operationid IN (12, 35, 150, 20);
      IF
        l_is_payment OR -- payment
        (:NEW.operationid IN (53) AND newamount < 0) OR -- internal offset < 0 (do like payment)
        (:NEW.operationid IN (227, 230))
      THEN
        l_old_curr_balance := ROUND(BS.trash_manager_2007_2.get_normalized_currbalance2(:NEW.custkey), 2);
      END IF;

      l_cust_balance := NVL(:NEW.balance, 0) + newamount;

      IF :NEW.operationid IN (19, 100, 213, 223, 80, 82, 228, 102) -- Zveli valis gadatana!!!
      THEN
        l_cust_old_balance   := NVL(:NEW.old_balance,  0) + newamount;
        l_cust_tech_balance  := NVL(:NEW.tech_balance, 0) + newamount;
        IF l_cust_old_balance < 0
        THEN
          l_cust_old_balance := 0;
        END IF;
      ELSIF :NEW.operationid IN (73, 129, 229, 600, 601, 602, 603, 103) -- MXOLOD mimdinare valis cvlileba!!!
      THEN
        ------------------------------------------------------------------------
        -- leilas werili!!! 30-Aug-2011 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ------------------------------------------------------------------------
        l_cust_old_balance   := :NEW.old_balance;
        l_cust_tech_balance  := :NEW.tech_balance;
      -- Zveli valis da kreditebis gasworeba
      ELSIF :NEW.operationid IN (trash_manager_2007_2.OLD_BALANCE_GE_0_OPERATION) -- Zveli balansis ganuleba (AMOUNT=0!)
      THEN
        l_cust_old_balance  := 0;
        l_cust_tech_balance := NVL(:NEW.tech_balance, 0);
      -- gadaxda mimdinareze metia!!!
      ELSIF l_old_curr_balance > 0  AND NVL(:NEW.old_balance,  0) > 0 AND newamount < 0 AND ABS(l_old_curr_balance) < ABS(newamount)
      THEN
        IF NVL(:NEW.old_balance, 0) + NVL(l_old_curr_balance, 0) < ABS(newamount)
        THEN
          l_cust_old_balance  := 0;
          l_cust_tech_balance := NVL(:NEW.tech_balance, 0) - NVL(:NEW.old_balance,  0);
        ELSE
          l_cust_old_balance   := NVL(:NEW.old_balance,  0)  + newamount + l_old_curr_balance;
          l_cust_tech_balance  := NVL(:NEW.tech_balance,  0) + newamount + l_old_curr_balance;
        END IF;
      -- Tu gadaxdaa da kreditSia mimdinare valiT, maSin gadaxda midis Zvelis gadaxdaze
      ELSIF l_old_curr_balance <= 0 AND NVL(:NEW.old_balance,  0) > 0 AND newamount < 0
      THEN
        IF NVL(:NEW.old_balance,  0) < ABS(newamount)
        THEN
          l_cust_old_balance  := 0;
          l_cust_tech_balance := NVL(:NEW.tech_balance, 0) - NVL(:NEW.old_balance,  0);
        ELSE
          l_cust_old_balance   := NVL(:NEW.old_balance,  0)  + newamount;
          l_cust_tech_balance  := NVL(:NEW.tech_balance,  0) + newamount;
        END IF;
      ELSE
        l_cust_old_balance  := NVL(:NEW.old_balance,  0);
        l_cust_tech_balance := NVL(:NEW.tech_balance, 0);
      END IF;

      UPDATE trashcustomer
      SET
        balance        =  ROUND(l_cust_balance, 2),
        old_balance    =  ROUND(l_cust_old_balance, 2),
        tech_balance   =  ROUND(l_cust_tech_balance, 2),
        curr_balance   = (ROUND(l_cust_balance, 2) - ROUND(l_cust_old_balance, 2)),
        discon_balance = (ROUND(l_cust_balance, 2) - ROUND(l_cust_old_balance, 2))
      WHERE
        custkey = :NEW.custkey;
    END IF;

    -- @since: 1-Aug-2013
    IF l_is_payment
    THEN
      l_old_amount := ROUND(l_cust_old_balance, 2) - :NEW.old_balance;
      l_new_amount := newamount - l_old_amount;

      IF ABS(l_old_amount) > 0.0099
      THEN
        INSERT INTO bs.trash_item_det (
          item_id, start_date, end_date, amnt_in_gel, enter_date, is_old
        ) VALUES (
          :NEW.trashitemid, :NEW.operdate, :NEW.operdate, l_old_amount, SYSDATE, 1
        );
      END IF;
      IF ABS(l_new_amount) > 0.0099
      THEN
        INSERT INTO bs.trash_item_det (
          item_id, start_date, end_date, amnt_in_gel, enter_date, is_old
        ) VALUES (
          :NEW.trashitemid, :NEW.operdate, :NEW.operdate, l_new_amount, SYSDATE, 0
        );
      END IF;
    END IF;
    -- @endsince

    IF :NEW.operationid = 209 THEN
      -- cancel trash account 
      UPDATE trashcustomer SET status = 2 WHERE custkey = :NEW.custkey;
    END IF;
  
  END IF;


END IF;

END;
