
  CREATE OR REPLACE TRIGGER "BS"."TRASHITEMID_BI" 
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
      newamount := - :NEW.amount;
    ELSE
      newamount := + :NEW.amount;
    END IF;

    IF :NEW.operationid != 208 THEN

      SELECT
        balance, old_balance, tech_balance, curr_balance
      INTO
        :NEW.balance, :NEW.old_balance, :NEW.tech_balance, l_old_curr_balance
      FROM
        trashcustomer
      WHERE custkey = :NEW.custkey;

      -- payment (use normilized current balance)
      IF
        :NEW.operationid IN (12, 35, 150, 20) OR -- payment
        (:NEW.operationid IN (53) AND newamount < 0) -- internal offset < 0 (do like payment)
      THEN
        l_old_curr_balance := BS.trash_manager_2007_2.get_normalized_currbalance2(:NEW.custkey);
      END IF;

      l_cust_balance      := NVL(:NEW.balance,      0) + newamount;

      IF :NEW.operationid IN (19, 100, 213, 223) -- Zveli valis gadatana!!!
      THEN
        l_cust_old_balance   := NVL(:NEW.old_balance,  0) + newamount;
        l_cust_tech_balance  := NVL(:NEW.tech_balance, 0) + newamount;
        IF l_cust_old_balance < 0
        THEN
          l_cust_old_balance := 0;
        END IF;
      -- Zveli valis dakreditebis gasworeba
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
        balance      = l_cust_balance,
        old_balance  = l_cust_old_balance,
        tech_balance = l_cust_tech_balance,
        curr_balance = (l_cust_balance - l_cust_old_balance)
      WHERE
        custkey = :NEW.custkey;

    END IF;

    IF :NEW.operationid = 209 THEN
      -- cancel trash account 
      UPDATE trashcustomer SET status = 2 WHERE custkey = :NEW.custkey;
    END IF;
  
  END IF;


END IF;

END;

ALTER TRIGGER "BS"."TRASHITEMID_BI" ENABLE
