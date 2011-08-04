create or replace PACKAGE                TRASH_MANAGER_2007_2 AS

  /**
   * This is a new version of BS.BILL_MANAGER_2007 package. We have updated the later
   * for two main reasons. First of all new billing procedures were proposed
   * and we added new functionality to the existing package. The second reason is
   * some slight problems in implementation of the old package, mainly these are
   * code repetitions accross different procedures, and also frequent use of
   * deprecated TARIFF_ID parameter from BS.TRASH_CUST_DET table.
   *
   * The content of BS.BILL_CORRECTIONS_2007 was also moved here. In fact there
   * is no big difference between common charging and correction calculation procedures.
   */

  /**
     <b>Glossary</b>
     of terms used in the documentation for the code of this package

     (*) ProcedureA -- charge procedure used from 1-Jan-2007 till 30-Sep-2007
     (*) ProcedureB -- charge procedure used since 1-Oct-2007

     **) NOTE: when we say that ProcedureB is used since 1-Oct-2007 we mean
     	 that we charged October using a new procedure, but procedure itself was
     	 started in November, 2007 

   */

-------------------------------- Custom Types ----------------------------------

/**
 * This is the type for month description. Here <code>p_first</code> and
 * <code>p_last</code> are the first and last dates of the month. Parameters
 * <code>p_detail_first</code> and <code>p_detail_last</code> are the first and
 * last dates for the month as they should be considered when calculating with
 * ProcedureA, this parameters are not used in ProcedureB.
 */
TYPE TP_MONTH IS RECORD (
  /**
   * Date of first day of the month (1-Jan, 1-Feb, etc.).
   */
  pFirst        DATE,
  /**
   * Date of last day of the month (31-Jan, 28/29-Feb, etc.).
   */
  pLast         DATE,
  /**
   * Date of first day of the month from where details history should be seen
   * by ProcedureA (16-Jan, 16-Feb, etc.).
   */
  pDetailFirst DATE,
  /**
   * Date of the last day of the month including which details history should
   * be seen by ProcedureA (15-Jan, 15-Feb, etc.).
   */
  pDetailLast  DATE
);

/**
 * This structure is used for charging a customer detail.
 */
TYPE TP_CHARGE_DETAIL IS RECORD (
  /**
   * Coefficient ID appropriate for this charge.
   */
  pCoeffId          NUMBER,
  /**
   * Customer detail ID appropriate for this charge.
   */
  pCustomerDetailId NUMBER,
  /**
   * Charge itself.
   */
  pGel              NUMBER,
  /**
   * Charge period start.
   */
  pStart            DATE,
  /**
   * Charge period end.
   */
  pEnd              DATE,
  /**
   * Detail amount.
   */
  pCustomerDetailAmount NUMBER,
  /**
   * Category/Unit mapping id.
   */
  pCustomerDetailCatUnitId NUMBER,
  /**
   * Number of cut days.
   */
  pCutDays NUMBER
);

/**
 * Array of TP_CHARGE_DETAILs.
 */
TYPE TP_CHARGE_DETAILS IS TABLE OF TP_CHARGE_DETAIL;

/**
 * This structure is used for charging a subsidy detail.
 */
TYPE TP_SUBSIDY_DETAIL IS RECORD (
  pSubsidyDetailId NUMBER,
  pCustomerDetailId NUMBER,
  pPersonCount NUMBER,
  pGel         NUMBER,
  pOperationId NUMBER,
  pStart DATE,
  pEnd DATE,
  /**
   * Number of cut days.
   */
  pCutDays NUMBER
);

/**
 * Array of TP_SUBSIDY_DETAILs.
 */
TYPE TP_SUBSIDY_DETAILS IS TABLE OF TP_SUBSIDY_DETAIL;

/**
 * This type represents full data obtained for customer calculation.
 */
TYPE TP_CALC_RES IS RECORD (
  pCharge TP_CHARGE_DETAILS,
  pSubsidy TP_SUBSIDY_DETAILS
);

/**
 * This structure is used when creating monmthly trash charge details.
 */
TYPE TP_CUSTOMER_SUMMARY IS RECORD (
  trash_cust_cat_id      NUMBER,
  pers_count             NUMBER,
  det_amount             NUMBER,
  start_debet            NUMBER,
  start_credit           NUMBER,
  start_old_balance      NUMBER,
  start_curr_debet     NUMBER,
  start_curr_credit      NUMBER,
  end_debet              NUMBER,
  end_credit             NUMBER,
  end_old_balance        NUMBER,
  end_curr_debet         NUMBER,
  end_curr_credit        NUMBER,
  last_item_id           NUMBER,
  prev_last_item_id      NUMBER,
  has_curr_subsidy       BOOLEAN,
  curr_subs_pers_count   NUMBER
);

/**
 * Date array.
 */
TYPE TP_DATE_ARRAY IS TABLE OF DATE;

----------------------------------- Constants ----------------------------------

/**
 * Minimal GEL charge to be considered valuable.
 */
MIN_GEL CONSTANT NUMBER := 0.0099;

/**
 * Unit id which defines persons count.
 */
UNIT_COUNT_OF_PERSONS CONSTANT NUMBER  := 2;

/**
 * In trash billing we have only one charge operation. This is the code of
 * charge operation.
 */
OPER_CHARGE CONSTANT NUMBER := 212;

/**
 * Previous period charge operation code. This operation looks like charge,
 * but actually we consider it like correction.
 */
OPER_PREV_PERIOD_CHARGE CONSTANT NUMBER := 214;

/**
 * Correction of charge operation code. We use it for correction of charge or
 * charge correction.
 */
OPER_CHARGE_CORRECTION CONSTANT NUMBER := 213;

/**
 * 50% subsidy operation code. For the moment (Sep, 2007) it is the only active
 * subsidy operation.
 */
OPER_SUBSIDY_50_PERCENT CONSTANT NUMBER := 221;

/**
 * 100% subsidy operation code. Actully it is not used for the moment
 * (Sep, 2007).
 */
OPER_SUBSIDY_100_PERCENT CONSTANT NUMBER  := 222;

/**
 * Previous period subsidy operation id. Like previous period charge operation
 * this operation is also in corrections group.
 */
OPER_PREV_PERIOD_SUBSIDY CONSTANT NUMBER := 226;

/**
 * Subsidy correction operation ID. Used for correction of subsidy and subsidy
 * corrections.
 */
OPER_SUBSIDY_CORRECTION CONSTANT NUMBER := 223;

/**
 * Operations used for correcting after 2011.
 */
OPER_CHARGE_CORR_AFTER_2011  CONSTANT NUMBER := 215;
OPER_SUBSIDY_CORR_AFTER_2011 CONSTANT NUMBER := 227;
CORR_START_DATE_2011 CONSTANT DATE := '1-Jan-2011';
OLD_BALANCE_GE_0_OPERATION CONSTANT NUMBER := 300;

/**
 * Category/unit mapping for Resident/Soul as it was in January 20007. It is not
 * required that current Resident/Soul mapping has the same ID.
 */
CUM_RESIDENT_JAN2007 CONSTANT NUMBER  := 1;

/**
 * Default calculation hint.
 */
HINT_DEFAULT CONSTANT NUMBER := 0;

/**
 * Hint which indicates calculation for producing voucher.
 */
HINT_AS_VOUCHER CONSTANT NUMBER := 1;

/**
 * ProcedureA name.
 */
NAME_PROCEDURE_A CONSTANT CHAR := 'A';

/**
 * ProcedureB name.
 */
NAME_PROCEDURE_B CONSTANT CHAR := 'B';

/**
 * Minimum supported correction date.
 */
MIN_CORRECTION_DATE CONSTANT DATE := '1-Jan-2007';

/**
 * Maximum supported correction date (need to be changed each month).
 */
MAX_CORRECTION_DATE CONSTANT DATE := '30-Jun-2011';

--------------------------- Monthly Summary Options ----------------------------

/**
 * Number used for Not-Defined category in monthly summaries.
 */
MONSUM_NOTDEFINED_CATEGORY CONSTANT NUMBER := -1;

/**
 * Number used for Residential category in monthly summaries.
 */
MONSUM_RESIDENTIAL_CATEGORY CONSTANT NUMBER := 0;

/**
 * Number used for Comercial category in monthly summaries.
 */
MONSUM_COMERCIAL_CATEGORY CONSTANT NUMBER := 1;

/**
 * Whether enable warnings when doing monthly summary.
 */
MONSUM_WARNINGS_ENABLED CONSTANT BOOLEAN := TRUE;

/**
 * Warning number used when no detail for trash customer
 * can be found.
 */
MONSUM_WARNING_NO_DETAIL CONSTANT NUMBER  := 1;

/**
 * Warning number used when no previous billing history for trash
 * customer can be found.
 */
MONSUM_WARNING_NO_PREV_HISTORY CONSTANT NUMBER  := 2;

/**
 * Warning number used when there are problems with current balance.
 */
MONSUM_WARNING_CURRBAL_PROBLEM CONSTANT NUMBER  := 3;

/**
 * Warning number used when item summary m^3 does not match what is written
 * in this item details.
 */
--balance_warn_m3_not_match       CONSTANT NUMBER  := 4;

/**
 * Warning number used when item summary GEL does not match what is written
 * in this item details.
 */
MONSUM_WARNING_GEL_NOT_MATCH CONSTANT NUMBER  := 5;

------------------------------ Charging Utilities ------------------------------

/**
 * Calculate normilized current balance value.
 */
FUNCTION get_normalized_currbalance(pCustomerId NUMBER, pItemId NUMBER DEFAULT NULL) RETURN NUMBER;
FUNCTION get_normalized_currbalance2(pCustomerId NUMBER) RETURN NUMBER;
/**
 * Creates TP_MONTH structure from date.
 */
PROCEDURE getMonth(p_date DATE, p_month OUT TP_MONTH);

/**
 * Calculate charge for the given month using ProcedureA.
 */
PROCEDURE calcMonthA (
  pCustomerId         NUMBER,
  pMonth              TP_MONTH,
  pHint    NUMBER,
  pResults OUT        TP_CALC_RES
);

/**
 * Calculate charge for a given customer and month using ProcedureB.
 */
PROCEDURE calcMonthB (
  pCustomerId  NUMBER,
  pMonth       TP_MONTH,
  pHint        NUMBER,
  pResults OUT TP_CALC_RES
);

/**
 * Run regular trash charge. According to current procedures this should
 */
PROCEDURE runRegularTrash(
  pMonthDate  DATE,
  pItemDate   DATE
);

----------------------------- Correction Utilities -----------------------------

PROCEDURE ensure_old_balance_ge_0 (p_customer_id NUMBER);
PROCEDURE editRecalculationDetail(pCustomerDetId NUMBER, pDisable NUMBER);
FUNCTION normalizeGel(pGel NUMBER, pOperationId NUMBER) RETURN NUMBER;
FUNCTION calcOldItemBalance (pCustomerId NUMBER, pItemId NUMBER, pPrevOldBalance NUMBER, pGel NUMBER, pOperationId NUMBER) RETURN NUMBER;

/**
 * Recalculates given month summaries: charge and subsidy GEL amounts.
 */
PROCEDURE recalculateMonth(
  pCustomerId     NUMBER,
  pMonth          TP_MONTH,
  pChargeGel  OUT NUMBER,
  pSubsidyGel OUT NUMBER
);

/**
 * Calculates existing charge and subsidies.
 */
PROCEDURE calculateExistingCharge(
  pCustomerId     NUMBER,
  pMonth          TP_MONTH,
  pChargeGel  OUT NUMBER,
  pSubsidyGel OUT NUMBER
);

/**
 * Recalculate.
 */
PROCEDURE recalculate(
  pCustomerId NUMBER,
  pStart      DATE := MIN_CORRECTION_DATE,
  pEnd        DATE := MAX_CORRECTION_DATE
);

/**
 * Recalculate and send to item.
 */
PROCEDURE recalculateAndSendToItem(
  pCustomerId NUMBER,
  pStart      DATE := MIN_CORRECTION_DATE,
  pEnd        DATE := MAX_CORRECTION_DATE
);

PROCEDURE recalculate_2010(pCustomerId NUMBER);
PROCEDURE recalculateAndSendToItem_2010(pCustomerId NUMBER);

/**
 * Runs trash summary.
 */
PROCEDURE runTrashSummary (pReportingDate DATE);

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

/**
 * Run all tests on this package.
 */
PROCEDURE test_all;

END TRASH_MANAGER_2007_2;
/

create or replace PACKAGE BODY            TRASH_MANAGER_2007_2 AS

------------------------------ Charging Utilities ------------------------------

/**
 * Subsidy cursor.
 */
CURSOR SUBSIDY_CURSOR(pCustomerId NUMBER, pStart DATE, pEnd DATE)
IS
  SELECT * FROM BS.TRASHSUBSIDIES
  WHERE
    ((fromdate <= pStart AND (todate IS NULL OR todate >= pStart))
    OR (fromdate <= pEnd AND (todate IS NULL OR todate >= pEnd)))
    AND custkey = pCustomerId
  ORDER BY
    -- OLD: active detail has more privelegies
    -- status ASC,
    trashsubsidyid DESC;

------------------------------ Trash Scheduling Calculations -------------------
-- @since May, 2011


/*FUNCTION month_schedule_date(pCustomerId NUMBER, pDate DATE)
RETURN DATE
IS
  pStart   DATE;
  pEnd     DATE;
  pAccount NUMBER;
  pSchedule NUMBER;
  pScheduleDate DATE;
  pBillDate DATE;
BEGIN
  pEnd := LAST_DAY(TRUNC(pDate));
  pStart := LAST_DAY( add_months(TRUNC(pDate), -1) ) + 1;

  BEGIN
    SELECT acckey INTO pAccount FROM bs.account WHERE custkey = pCustomerId AND mainaccount = 1;
  EXCEPTION WHEN no_data_found THEN
    RETURN NULL; -- customer with no main account
  END;

  -- searching schedule information in given month

  SELECT MAX(schedkey) INTO pSchedule
  FROM bs.item
  WHERE acckey = pAccount AND schedkey IS NOT NULL AND itemdate BETWEEN pStart AND pEnd;

  IF pSchedule IS NOT NULL
  THEN
    SELECT cycledate INTO pScheduleDate FROM bs.schedule WHERE schedkey = pSchedule;
    RETURN TRUNC(pScheduleDate);
  END IF;

  -- search bill_item for schedule date

  --SELECT MAX(DATE) INTO pBillDate

  -- schedule not found in this month: get last schedule date (from previous months)

  SELECT MAX(schedkey) INTO pSchedule
  FROM bs.item
  WHERE acckey = pAccount AND schedkey IS NOT NULL AND itemdate < pStart;

  IF pSchedule IS NOT NULL
  THEN
    SELECT cycledate INTO pScheduleDate FROM bs.schedule WHERE schedkey = pSchedule;
    RETURN TRUNC(pScheduleDate);
  END IF;

  RETURN NULL;
END month_schedule_date;*/

/**
 * @pHint 
 *   0: normal
 *   1: main account not found
 *   2: future date, date too new
 *   3: schedule not found
 */
PROCEDURE month_schedule_date(
  pCustomerId   IN  NUMBER,
  pDate         IN  DATE,
  pScheduleDate OUT DATE,
  pHint         OUT NUMBER
) IS
  l_account NUMBER;
  l_start   DATE;
  l_end     DATE;
  l_curr_month BOOLEAN;
  l_prev_month BOOLEAN;
BEGIN
  pHint := 0;
  pScheduleDate := null;

  -- getting main account information
  BEGIN
    SELECT acckey INTO l_account FROM bs.account WHERE custkey = pCustomerId AND mainaccount = 1;
  EXCEPTION WHEN no_data_found THEN
    pHint := 1; -- customer with no main account!!!
    pScheduleDate := NULL;
    RETURN;
  END;

  -- getting month information
  l_end := LAST_DAY(TRUNC(pDate));
  l_start := LAST_DAY( add_months(TRUNC(pDate), -1) ) + 1;
  l_curr_month := ABS(LAST_DAY(TRUNC(sysdate)) - l_end) < 1;
  l_prev_month := ABS(LAST_DAY( LAST_DAY( add_months(TRUNC(sysdate), - 2) ) + 1) - l_end) < 1;

  -- date is a far future for proper processing
  IF pDate > sysdate + 10
  THEN
    pHint := 2;
    pScheduleDate := NULL;
    RETURN;
  END IF;

  -- get current month information from schedule
  IF l_curr_month OR l_prev_month
  THEN
    -- try to find schedule for this month
    SELECT MAX(s.cycledate) INTO pScheduleDate
    FROM
      bs.schedule s inner join bs.routeacc ra on s.routekey = ra.routekey
    WHERE
      ra.acckey = l_account AND s.cycledate BETWEEN l_start AND l_end;
    IF pScheduleDate IS NOT NULL
    THEN
      pHint := 0;
      RETURN;
    END IF;
    -- current month schedule not yet generated: lookup previous month
    DECLARE
      l_prev_start DATE;
      l_prev_end   DATE;
    BEGIN
      l_prev_end   := l_start - 1;
      l_prev_start := LAST_DAY( add_months(TRUNC(pDate), -2) ) + 1;
      SELECT MAX(s.cycledate) INTO pScheduleDate
      FROM
        bs.schedule s inner join bs.routeacc ra on s.routekey = ra.routekey
      WHERE
        ra.acckey = l_account AND s.cycledate BETWEEN l_prev_start AND l_prev_end;
      IF pScheduleDate IS NOT NULL
      THEN
        pHint := 0;
        RETURN;
      END IF;
    END;
  END IF;
/*
  -- lookup in bill_item
  SELECT MAX(billdate) INTO pScheduleDate
  FROM bs.item_bill
  WHERE acckey = l_account AND billdate BETWEEN l_start AND l_end;
  IF pScheduleDate IS NOT NULL
  THEN
    pHint := 0;
    RETURN;
  END IF;
*/
/*  -- try use schedule again (MAX 60 days interval from now)
  IF ABS(l_start - sysdate) < 90
  THEN
    SELECT MAX(s.cycledate) INTO pScheduleDate
    FROM
      bs.schedule s inner join bs.routeacc ra on s.routekey = ra.routekey
    WHERE
      ra.acckey = l_account AND s.cycledate BETWEEN l_start AND l_end;
    IF pScheduleDate IS NOT NULL
    THEN
      pHint := 0;
      RETURN;
    END IF;
  END IF;*/

  -- no schedule found
  pHint := 3;
  pScheduleDate := NULL;
END month_schedule_date;

FUNCTION get_normalized_currbalance2(pCustomerId NUMBER)
RETURN NUMBER
IS
  l_cut_date DATE;
  l_balance NUMBER;
BEGIN
  l_cut_date :=  last_day(add_months(sysdate, -3)) + 1;
  SELECT curr_balance INTO l_balance FROM bs.trashcustomer WHERE custkey = pCustomerId;
  FOR item IN (
    SELECT * FROM bs.trashitem WHERE custkey = pCustomerId AND enterdate > l_cut_date
    ORDER BY trashitemid DESC
  ) LOOP
    IF item.isprinted = 1
    THEN
      EXIT;
    END IF;
    IF item.operationid IN (212, 215, 221, 222, 227)
    THEN
      l_balance := l_balance - normalizeGel(item.amount, item.operationid);
    END IF;
  END LOOP;
  RETURN l_balance;
END;

/**
 * Calculate normilized current balance.
 *
 * @param pCustomerId customer identificator
 * @param pItemId item identificator which should be calculated by this procedure;
 *                use <code>NULL</code> value when processing new operation
 */
FUNCTION get_normalized_currbalance(pCustomerId NUMBER, pItemId NUMBER DEFAULT NULL)
RETURN NUMBER
IS
  l_oper_date DATE;
  l_sched_date DATE;
  l_hint NUMBER;
  l_curr_balance NUMBER;
BEGIN

  IF pItemId IS NULL
  THEN
    l_oper_date := TRUNC(sysdate);
    SELECT curr_balance INTO l_curr_balance FROM bs.trashcustomer
    WHERE custkey = pCustomerId;
  ELSE
    SELECT ENTERDATE INTO l_oper_date
    FROM bs.trashitem WHERE trashitemid = pItemId;
    BEGIN
      SELECT ( balance - old_balance ) INTO l_curr_balance
      FROM (SELECT balance, old_balance FROM bs.trashitem
        WHERE custkey = pCustomerId AND trashitemid > pItemId
        ORDER BY trashitemid ASC)
      WHERE ROWNUM = 1;
    EXCEPTION WHEN no_data_found
    THEN
      SELECT curr_balance INTO l_curr_balance FROM bs.trashcustomer
      WHERE custkey = pCustomerId;
    END;
  END IF;

  -- get schedule date for this month
  month_schedule_date(pCustomerId, l_oper_date, l_sched_date, l_hint);

  IF l_hint IN (1, 2, 3)
  THEN
    RETURN l_curr_balance;
  END IF;

  IF l_oper_date < l_sched_date
  THEN
    month_schedule_date(pCustomerId, ADD_MONTHS(l_oper_date, -1), l_sched_date, l_hint);
    IF l_hint IN (1, 2, 3)
    THEN
      RETURN l_curr_balance;
    END IF;
  END IF;

  -- move up until schedule date
  FOR item IN (
    SELECT * FROM trashitem
    WHERE custkey = pCustomerId AND
          enterdate > l_sched_date AND
          trashitemid <= NVL(pItemId, 999999999999999)
  ) LOOP
    IF item.operationid IN (212, 215, 221, 222, 227)
    THEN
      l_curr_balance := l_curr_balance - normalizeGel(item.amount, item.operationid);
    END IF;
  END LOOP;

  RETURN l_curr_balance;

END get_normalized_currbalance;

--------------------------------------------------------------------------------

/**
 * Returns name of the procedure which is used for the given month and calculation type.
 */
FUNCTION getProcedureName (
  pMonth TP_MONTH,
  pHint NUMBER
) RETURN CHAR
IS
BEGIN

  -- beyond scope of our procedures
  IF pMonth.pFirst < '1-Jan-2007'
  THEN
    RETURN 'X';
  -- voucher calculation
  ELSIF NVL(pHint, -1) = HINT_AS_VOUCHER
  THEN
    RETURN NAME_PROCEDURE_B;
  -- ProcedureA
  ELSIF pMonth.pFirst < '1-Oct-2007'
  THEN
    RETURN NAME_PROCEDURE_A;
  -- ProcedureB
  ELSE
    RETURN NAME_PROCEDURE_B;
  END IF;

END getProcedureName;

/**
 * Creates TP_MONTH structure appropriate to the parameter date.
 */
PROCEDURE getMonth(p_date DATE, p_month OUT TP_MONTH)
IS
  p_first            DATE;
  p_last             DATE;
  p_critical_start   DATE;
  p_critical_end     DATE;
BEGIN
  -- from 16 PreviousMonth to 15 CurrentMonth
  p_month.pDetailFirst :=TRUNC (LAST_DAY (ADD_MONTHS (p_date, -2))) + 15 + 1;
  p_month.pDetailLast := TRUNC (LAST_DAY (ADD_MONTHS (p_date, -1))) + 15;
  -- from 1 CurrentMonth to LastDay CurrentMonth
  p_month.pFirst := TRUNC (LAST_DAY (ADD_MONTHS (p_date, -1))) + +1;
  p_month.pLast := TRUNC (LAST_DAY (p_date));
END;

/**
 * Append charge into charges array.
 */
PROCEDURE appendCharge(
  pChargeDetail TP_CHARGE_DETAIL,
  pCharges IN OUT TP_CHARGE_DETAILS
) IS
BEGIN

  -- create charge details if nothing exists before
  IF pCharges IS NULL
  THEN
    pCharges := TP_CHARGE_DETAILS();
  END IF;

  pCharges.EXTEND;
  pCharges(pCharges.COUNT) := pChargeDetail;

END appendCharge;

/**
 * Append charge into calculation results.
 */
PROCEDURE appendCharge(
  pChargeDetail TP_CHARGE_DETAIL,
  pResult IN OUT TP_CALC_RES
) IS
BEGIN
  appendCharge(pChargeDetail, pResult.pCharge);
END appendCharge;

/**
 * Appends multiple charge details into calculation result.
 */
PROCEDURE appendCharges(
  pChargeDetails TP_CHARGE_DETAILS,
  pResult IN OUT TP_CALC_RES
) IS
  p_counter NUMBER := 1;
BEGIN

  IF pChargeDetails IS NOT NULL
  THEN
    WHILE p_counter <= pChargeDetails.COUNT
    LOOP
      appendCharge(pChargeDetails(p_counter), pResult);
      p_counter := p_counter + 1;
    END LOOP;
  END IF;

END appendCharges;

/**
 * Append subsidy into subsidies array.
 */
PROCEDURE appendSubsidy(
  pSubsidyDetail TP_SUBSIDY_DETAIL,
  pSubsidies IN OUT TP_SUBSIDY_DETAILS
) IS
BEGIN
  IF pSubsidies IS NULL
  THEN
    pSubsidies := TP_SUBSIDY_DETAILS();
  END IF;
  
  pSubsidies.EXTEND;
  pSubsidies(pSubsidies.COUNT) := pSubsidyDetail;
  
END appendSubsidy;

/**
 * Append subsidy in calculation results.
 */
PROCEDURE appendSubsidy(
  pSubsidyDetail TP_SUBSIDY_DETAIL,
  pResult IN OUT TP_CALC_RES
) IS
BEGIN
  appendSubsidy(pSubsidyDetail, pResult.pSubsidy);
END appendSubsidy;

/**
 * Append subsidy.
 */
PROCEDURE appendSubsidy(
  pSubsidyDetails TP_SUBSIDY_DETAILS,
  pResult  IN OUT TP_CALC_RES
) IS
  pCounter NUMBER := 1;
BEGIN
  IF pSubsidyDetails IS NOT NULL
  THEN
    WHILE pCounter <= pSubsidyDetails.COUNT
    LOOP
      appendSubsidy(pSubsidyDetails(pCounter), pResult);
      pCounter := pCounter + 1;
    END LOOP;
  END IF;
END appendSubsidy;

/**
 * Find intersection of main with detail intervals.
 */
PROCEDURE findIntersection(
  pStart        DATE,
  pEnd          DATE,
  pDetailStart  DATE,
  pDetailEnd    DATE,
  pIntersection OUT BOOLEAN,
  pD1           OUT DATE,
  pD2           OUT DATE
) IS
BEGIN

  -- getting detail dates interval
  -- #1: d1 .. D1 .. D2 .. d2
  IF
    pDetailStart <= pStart AND -- d1 .. D1
    (pDetailEnd IS NULL OR (pDetailEnd IS NOT NULL AND pDetailEnd >= pEnd)) -- D2 .. d2
  THEN
    pD1 := pStart;
    pD2 := pEnd;
    pIntersection := TRUE;
  -- #2: D1 .. d1 .. d2 .. D2
  ELSIF
    pStart <= pDetailStart AND -- D1 .. d1
    (pDetailEnd IS NOT NULL AND pDetailEnd <= pEnd) -- d2 .. D2
  THEN
    pD1 := pDetailStart;
    pD2 := pDetailEnd;
    pIntersection := TRUE;
  -- #3: d1 .. D1 .. d2 .. D2
  ELSIF
    pDetailStart <= pStart AND -- d1 .. D1
    (pDetailEnd IS NOT NULL AND pDetailEnd <= pEnd AND pDetailEnd >= pStart) -- D1 .. d2 .. D2
  THEN
    pD1 := pStart;
    pD2 := pDetailEnd;
    pIntersection := TRUE;
  -- #4: D1 .. d1 .. D2 .. d2
  ELSIF
    (pDetailStart >= pStart AND (pEnd IS NULL OR pEnd >= pDetailStart))AND -- D1 .. d1 .. D2
    (pDetailEnd IS NULL OR pDetailEnd >= pEnd) -- D2 .. d2
  THEN
    pD1 := pDetailStart;
    pD2 := pEnd;
    pIntersection := TRUE;
  ELSE
    pIntersection := FALSE;
  END IF;

END findIntersection;

/**
 * Calculates single customer detail record using to ProcedureA.
 *
 * @param pCategoryUnitId category unit mapping id for which we need to calculate
 * @param pCustomerDetailAmount amount in customer detail record
 * @param pMonth month when calculating
 * @param pGel resulting GEL
 * @param pCategoryDetailId id of the category detail used in calculation
 */
PROCEDURE calcByCoeffDetailsA (
  pCategoryUnitId       NUMBER,
  pCustomerDetailAmount NUMBER,
  pMonth                TP_MONTH,
  pGel           OUT NUMBER,
  pCoeffDetailId OUT NUMBER
) IS
  pTariff NUMBER;
BEGIN

  -- getting category row
  BEGIN
    SELECT id, monthly_traiff_val INTO pCoeffDetailId, pTariff FROM (
      SELECT id, monthly_traiff_val FROM bs.trash_cat_coeffs
      WHERE cat_unit_id = pCategoryUnitId
        AND ((start_date <= pMonth.pDetailFirst AND (end_date IS NULL OR end_date >= pMonth.pDetailFirst))
          OR (start_date <= pMonth.pDetailLast AND (end_date IS NULL OR end_date >= pMonth.pDetailLast))
    ) ORDER BY ID DESC)
    WHERE ROWNUM = 1;
  EXCEPTION WHEN NO_DATA_FOUND
  THEN
    pTariff := NULL;
  END;

  -- calculating
  IF pTariff IS NOT NULL
  THEN

    -- when January 2007 resident/soul charge should be devided by 2 (half month)
    IF pMonth.pFirst = '01-Jan-2007' AND pCategoryUnitId = CUM_RESIDENT_JAN2007
    THEN
      pGel := pCustomerDetailAmount * pTariff / 2;
    ELSE
      pGel := pCustomerDetailAmount * pTariff;
    END IF;
  ELSE
    pGel := NULL;
    pCoeffDetailId := NULL;
  END IF;

END calcByCoeffDetailsA;

/**
 * Calculate charge by coeff details according to the ProcedureB. Note that here
 * we calculate using many coeff details, not a single one, as it was in
 * ProcedureA. Also note pStartDate and pEndDate parameters: they normally are
 * parameters which describe start and end of the underleying customer detail.
 */
PROCEDURE calcByCoeffDetailsB (
  pCategoryUnitId       NUMBER,
  pCustomerDetailAmount NUMBER,
  pCustomerDetailId     NUMBER,
  pCustomerDetailCatUnitId NUMBER,
  pStartDate            DATE,
  pEndDate              DATE,
  pMonth                TP_MONTH,
  pCharges           OUT TP_CHARGE_DETAILS
) IS
  pNewStartDate DATE;
  pNewEndDate DATE;
  pD1 DATE;
  pD2 DATE;
  pLastEndDate DATE;
  pDaysInMonth NUMBER;
  pInterval NUMBER;
  pGel NUMBER;
  pContinue BOOLEAN;
  pCharge TP_CHARGE_DETAIL;
BEGIN
  -- adjusting start and end dates
  pNewStartDate := NVL(pStartDate, pMonth.pFirst);
  pNewEndDate := NVL(pEndDate, pMonth.pLast);

  -- month interval
  pDaysInMonth := TRUNC(pMonth.pLast) - TRUNC(pMonth.pFirst) + 1;
  -- initialize last-end-date
  pLastEndDate := pNewStartDate;
  -- looping over all releavant coeff items
  FOR rec IN (
    SELECT * FROM BS.TRASH_CAT_COEFFS
    WHERE
      ((start_date <= pNewStartDate AND (end_date IS NULL OR end_date >= pNewStartDate))
        OR (start_date <= pNewEndDate AND (end_date IS NULL OR end_date >= pNewEndDate)) )
        AND CAT_UNIT_ID = pCategoryUnitId
    ORDER BY ID ASC
  ) LOOP
    -- check interval
    IF pLastEndDate <= pNewEndDate
    THEN
      -- find intersection (pD1, pD2)
      findIntersection(pLastEndDate, pNewEndDate, rec.start_date, rec.end_date, pContinue, pD1, pD2);

      -- calculate this detail
      IF pContinue AND TRUNC(pD1) <= TRUNC(pD2)
      THEN
        pInterval := TRUNC(pD2) - TRUNC(pD1) + 1;
        pGel := pInterval / pDaysInMonth * rec.monthly_traiff_val * pCustomerDetailAmount;

        -- divide by 2 for January, residential category
        IF pMonth.pFirst = '1-Jan-2007' AND pCategoryUnitId = CUM_RESIDENT_JAN2007
        THEN
          pGel := pGel / 2;
        END IF;

        -- XXX: This was removed at 27/05/2008. They need zeros like other
        -- (non-zero) charges.
        --IF ABS(pGel) >= MIN_GEL
        --THEN
          pCharge.pCoeffId := rec.ID;
          pCharge.pCustomerDetailId := pCustomerDetailId;
          pCharge.pGel := pGel;
          pCharge.pStart := pD1;
          pCharge.pEnd := pD2;
          pCharge.pCustomerDetailAmount := pCustomerDetailAmount;
          pCharge.pCustomerDetailCatUnitId := pCustomerDetailCatUnitId;
          appendCharge(pCharge, pCharges);
        --END IF;
      END IF;

      -- adjusting last-end-date for next step in loop
      pLastEndDate := pD2 + 1;
    END IF;

  END LOOP;

END calcByCoeffDetailsB;

/**
 * Append date into array keeping elements sorted and unique.
 */
PROCEDURE appendDateSortedAndUnique(
  pDate  IN DATE,
  pDates IN OUT TP_DATE_ARRAY
) IS
  pIndex NUMBER;
  pIndex2 NUMBER;
  pDiff NUMBER;
  pAppended BOOLEAN;
BEGIN
  IF pDates IS NULL
  THEN
    pDates := TP_DATE_ARRAY();
  END IF;

  pIndex := 1;
  pAppended := FALSE;
  WHILE pIndex <= pDates.COUNT
  LOOP
    pDiff := TRUNC(pDate) - pDates(pIndex);
    IF pDiff = 0 -- this date is already in the array!
    THEN
      pAppended := TRUE;
      EXIT;
    ELSIF pDiff < 0 -- this element should be placed at pIndex position
    THEN
      -- move all upper element futher level up
      pDates.EXTEND;
      pIndex2 := pDates.COUNT;
      WHILE pIndex2 > pIndex
      LOOP
        pDates(pIndex2) := pDates(pIndex2 - 1);
        pIndex2 := pIndex2 - 1;
      END LOOP;
      -- place date in appropriate position
      pDates(pIndex) := pDate;
      pAppended := TRUE;
      EXIT;
    END IF;
    pIndex := pIndex + 1;
  END LOOP;
  
  -- append if not appended
  IF NOT pAppended
  THEN
    pDates.EXTEND;
    pDates(pDates.COUNT) := pDate;
  END IF;
  
END appendDateSortedAndUnique;

/**
 * Get array of critical dates for the customer in given charge interval.
 */
FUNCTION getSubsidyChargeCriticalDates(
  pCustomerId NUMBER,
  pCh1 DATE, -- charge interval start
  pCh2 DATE -- charge interval end
) RETURN TP_DATE_ARRAY  IS
  pD1 DATE;
  pD2 DATE;
  pContinue BOOLEAN;
  pDates TP_DATE_ARRAY;
BEGIN

  -- looping over all subsidy details
  FOR det IN SUBSIDY_CURSOR(pCustomerId, pCh1, pCh2)
  LOOP
  
    -- find intersection
    findIntersection(pCh1, pCh2, det.fromdate, det.todate, pContinue, pD1, pD2);
    
    -- when interval is correct
    IF pContinue AND TRUNC(pD1) <= TRUNC(pD2)
    THEN
      appendDateSortedAndUnique(pD1, pDates);
      appendDateSortedAndUnique(pD2, pDates);
    END IF;
  
  END LOOP;
  
  RETURN pDates;

END getSubsidyChargeCriticalDates;

/**
 * This is a new version of calcSubsidyB procedure.
 *
 * @since Arpil, 2009
 */
PROCEDURE calcSubsidyB_2009 (
  pCustomerId    NUMBER,
  pCharge        TP_CHARGE_DETAIL,
  pSubsidies OUT TP_SUBSIDY_DETAILS
) IS
  pDates TP_DATE_ARRAY;
  pFullInterval NUMBER;
BEGIN
  
  -- when subsidy cannot be calculated
  IF (
       ABS(NVL(pCharge.pGel, 0)) < MIN_GEL -- empty charge: nothing to calculate
       OR
       pCharge.pCustomerDetailAmount < 1 -- probably not-residential category
  ) THEN
    RETURN;
  END IF;
  
  -- get subsidy critical dates
  pDates := getSubsidyChargeCriticalDates(pCustomerId, pCharge.pStart, pCharge.pEnd);
  
  -- exit calculation if critical dates are empty or not enough
  IF pDates IS NULL OR pDates.COUNT < 2
  THEN
    RETURN;
  END IF;
  
  -- full interval
  pFullInterval := TRUNC(pCharge.pEnd) - TRUNC(pCharge.pStart) + 1;
  
  -- calculate subsidy
  DECLARE
    pIndex NUMBER;
    pD1 DATE;
    pD2 DATE;
    pInterval NUMBER;
    pSoulCount NUMBER;
    pNewAmount NUMBER;
    pGel NUMBER;
    pSubsidy TP_SUBSIDY_DETAIL;
  BEGIN
    pIndex := 2;
    WHILE pIndex <= pDates.COUNT
    LOOP
      -- get interval dates
      pD1 := pDates(pIndex - 1);
      pD2 := pDates(pIndex);
      pInterval := TRUNC(pD2) - TRUNC(pD1) + 1;
      
      -- initial value for soul count
      pSoulCount := pCharge.pCustomerDetailAmount;
      
      -- loop over subsidy details of this customer in the given interval
      FOR det IN SUBSIDY_CURSOR(pCustomerId, pD1, pD2)
      LOOP
        
        -- get new amount
        pNewAmount := pSoulCount;
        IF pNewAmount > NVL(det.quantity, 0)
        THEN
          pNewAmount := NVL(det.quantity, 0);
          pSoulCount := pSoulCount - det.quantity;
        ELSE
          pSoulCount := 0;
        END IF;

        -- XXX: calculate GEL
        IF NVL(pNewAmount, 0) = 0
        THEN
          -- this case can not be calculated
          pGel := 0;
        ELSIF det.operationid = OPER_SUBSIDY_50_PERCENT
          AND NVL(det.quantity, 0) > 0
        THEN
          pGel := -0.5 * pCharge.pGel * (pInterval/pFullInterval) * (pNewAmount/pCharge.pCustomerDetailAmount);
        ELSIF det.operationid = OPER_SUBSIDY_50_PERCENT
          AND NVL(det.quantity, 0) > 0
        THEN
          pGel := -1.0 * pCharge.pGel * (pInterval/pFullInterval) * (pNewAmount/pCharge.pCustomerDetailAmount);
        END IF;

        -- make subsidy detail and append it to output
        IF ABS(pGel) >= MIN_GEL
        THEN
          pSubsidy.pSubsidyDetailId := det.TRASHSUBSIDYID;
          pSubsidy.pCustomerDetailId := pCharge.pCustomerDetailId;
          pSubsidy.pPersonCount := pNewAmount;
          pSubsidy.pGel := pGel;
          pSubsidy.pOperationId := det.operationid;
          pSubsidy.pStart := TRUNC(pD1);
          pSubsidy.pEnd := TRUNC(pD2);
          appendSubsidy(pSubsidy, pSubsidies);
        END IF;
        
      END LOOP;
      
      -- next step
      pIndex := pIndex + 1;
    END LOOP;
  END;
  
END calcSubsidyB_2009;

/**
 * Calculate subsidy for given charge using ProcedureB.
 *
 * Charge parameter (pCharge) gives the interval for consideration, charged
 * amount itself and number of souls this charge was calculated for. Subsidy
 * calculation is then done using charge interval and subsidy details
 * intersection. If two subsidy details for the given interval have intersection,
 * then only the first (order is determined in subsidy cursor) subsidy detail 
 * is considered for the intersection period; the second subsidy detail is
 * considered ...
 */
PROCEDURE calcSubsidyB (
  pCustomerId    NUMBER,
  pCharge        TP_CHARGE_DETAIL,
  pSubsidies OUT TP_SUBSIDY_DETAILS
) IS
  pNewEndDate DATE;
  pLastEndDate DATE;
  pD1 DATE;
  pD2 DATE;
  pContinue BOOLEAN;
  pFullInterval NUMBER;
  pInterval NUMBER;
  pNewAmount NUMBER;
  pGel NUMBER;
  pSubsidy TP_SUBSIDY_DETAIL;
BEGIN

  -- empty charge: nothing calculate here
  IF ABS(NVL(pCharge.pGel, 0)) < MIN_GEL 
  -- probably not-residential category
  OR pCharge.pCustomerDetailAmount < 1
  THEN
    RETURN;
  END IF;

  pNewEndDate := pCharge.pEnd;
  pLastEndDate := pCharge.pStart;
  pFullInterval := TRUNC(pCharge.pEnd) - TRUNC(pCharge.pStart) + 1;

  -- loop over subsidy details of this customer
  FOR det IN SUBSIDY_CURSOR(pCustomerId, pCharge.pStart, pCharge.pEnd)
  LOOP

    -- check interval
    IF pLastEndDate <= pNewEndDate
    THEN
      -- find intersection (pD1, pD2)
      findIntersection(pLastEndDate, pNewEndDate, det.fromdate, det.todate, pContinue, pD1, pD2);

      -- when interval is correct
      IF pContinue AND TRUNC(pD1) <= TRUNC(pD2)
      THEN
        -- interval
        pInterval := TRUNC(pD2) - TRUNC(pD1) + 1;
        -- determine new amount
        pNewAmount := pCharge.pCustomerDetailAmount;
        IF pNewAmount > NVL(det.quantity, 0)
        THEN
          pNewAmount := NVL(det.quantity, 0);
        END IF;

        -- calculate GEL
        IF NVL(pCharge.pCustomerDetailAmount, 0) = 0
        THEN
          -- this case can not be calculated
          pGel := 0;
        ELSIF det.operationid = OPER_SUBSIDY_50_PERCENT
          AND NVL(det.quantity, 0) > 0
        THEN
          pGel := -0.5 * pCharge.pGel * (pInterval/pFullInterval) * (pNewAmount/pCharge.pCustomerDetailAmount);
        ELSIF det.operationid = OPER_SUBSIDY_50_PERCENT
          AND NVL(det.quantity, 0) > 0
        THEN
          pGel := -1.0 * pCharge.pGel * (pInterval/pFullInterval) * (pNewAmount/pCharge.pCustomerDetailAmount);
        END IF;

        -- make subsidy detail and append it to output
        IF ABS(pGel) >= MIN_GEL
        THEN
          pSubsidy.pSubsidyDetailId := det.TRASHSUBSIDYID;
          pSubsidy.pCustomerDetailId := pCharge.pCustomerDetailId;
          pSubsidy.pPersonCount := pNewAmount;
          pSubsidy.pGel := pGel;
          pSubsidy.pOperationId := det.operationid;
          pSubsidy.pStart := TRUNC(pD1);
          pSubsidy.pEnd := TRUNC(pD2);
          appendSubsidy(pSubsidy, pSubsidies);
        END IF;

      END IF;

      -- last end date
      pLastEndDate := pD2 + 1;

    END IF;

  END LOOP;

END calcSubsidyB;

/**
 * Applies cuts on calculation results.
 * Month parameter is needed as a hint.
 */
PROCEDURE applyCuts(
  pCustomerId    NUMBER,
  pMonth         TP_MONTH,
  pResult IN OUT TP_CALC_RES
) IS
  TYPE TP_CUT_INTERVALS IS TABLE OF BS.TRASH_CUST_CUTS%ROWTYPE;
  pCounter NUMBER;
  pCharge  TP_CHARGE_DETAIL;
  pSubsidy TP_SUBSIDY_DETAIL;
  pCutIntervals TP_CUT_INTERVALS := TP_CUT_INTERVALS();
  pD1 DATE;
  pD2 DATE;
  pIntersection BOOLEAN;
  pFullInterval NUMBER;
  pCounter2 NUMBER;
  pLoopCharge BOOLEAN := FALSE;
  pLoopSubsidy BOOLEAN := FALSE;
BEGIN
  -- if no charge then why we need to cut?
  IF pResult.pCharge IS NULL OR pResult.pCharge.COUNT = 0
  THEN
    RETURN;
  END IF;

  -- loop over all cuts in this month
  FOR cut IN (
    SELECT * FROM BS.TRASH_CUST_CUTS
    WHERE CUSTOMER_ID = pCustomerId
      AND ((start_date <= pMonth.pFirst AND (end_date IS NULL OR end_date >= pMonth.pFirst))
          OR (start_date <= pMonth.pLast AND (end_date IS NULL OR end_date >= pMonth.pLast)))
    ORDER BY CAT_UNIT_ID
  ) LOOP
    -- initialize charge items counter
    pCounter := 1;

    -- save cut interval for future validation
    pCutIntervals.EXTEND;
    pCutIntervals(pCutIntervals.COUNT) := cut;

    -- loop over charge details
    WHILE pCounter <= pResult.pCharge.COUNT
    LOOP
      -- getting charge
      pCharge := pResult.pCharge(pCounter);
      
      -- look for matching category/unit mapping id
      -- note that when cut.CAT_UNIT_ID IS NULL, then it can be applied for any category/unit mapping
      IF cut.CAT_UNIT_ID IS NULL OR cut.CAT_UNIT_ID = NVL(pCharge.pCustomerDetailCatUnitId, -1)
      THEN
        -- calculate cut
        findIntersection(pCharge.pStart, pCharge.pEnd, cut.START_DATE,
          cut.END_DATE, pIntersection, pD1, pD2);
        IF pIntersection
        THEN
          pLoopCharge := TRUE; -- we need to recalculate charge
          pCharge.pCutDays := NVL(pCharge.pCutDays, 0) + (pD2 - pD1 + 1);
          pResult.pCharge(pCounter) := pCharge;
          --<<< now loop over subsidies
          IF pResult.pSubsidy IS NOT NULL -- XXX: here was condition without NOT
          THEN
            pCounter2 := 1;
            WHILE pCounter2 <= pResult.pSubsidy.COUNT
            LOOP
              pSubsidy := pResult.pSubsidy(pCounter2);
              IF NVL(pSubsidy.pCustomerDetailId, -1) = NVL(pCharge.pCustomerDetailId, -1)
              THEN
                findIntersection(pSubsidy.pStart, pSubsidy.pEnd, cut.START_DATE,
                  cut.END_DATE, pIntersection, pD1, pD2);
                IF pIntersection
                THEN
                  pLoopSubsidy := TRUE; -- we need to recalculate subsidy
                  pSubsidy.pCutDays := NVL(pSubsidy.pCutDays, 0) + (pD2 - pD1 + 1);
                  pResult.pSubsidy(pCounter2) := pSubsidy;
                END IF;
              END IF;
              pCounter2 := pCounter2 + 1;
            END LOOP;
          END IF;
          -->>>
        END IF;
      END IF;

      -- next step
      pCounter := pCounter + 1;
    END LOOP; -- charges loop

  END LOOP; -- cuts loop

  IF pLoopCharge OR pLoopSubsidy
  THEN
    -- TODO: validate pCutIntervals on intersections!!!!!!!!!!!!!!!!!!!!!!
    -- there should be NONE
    NULL;
  END IF;

  -- recalculate charge
  IF pLoopCharge
  THEN
    pCounter := 1;
    WHILE pCounter <= pResult.pCharge.COUNT
    LOOP
      pCharge := pResult.pCharge(pCounter);
      IF NVL(pCharge.pCutDays, 0) > 0
      THEN
        pCharge.pGel := pCharge.pGel * (1 - NVL(pCharge.pCutDays, 0)/(pCharge.pEnd - pCharge.pStart + 1));
        pResult.pCharge(pCounter) := pCharge;
      END IF;
      pCounter := pCounter + 1;
    END LOOP;
  END IF;

  -- recalculate subsidy
  IF pLoopSubsidy
  THEN
    pCounter := 1;
    WHILE pCounter <= pResult.pSubsidy.COUNT
    LOOP
      pSubsidy := pResult.pSubsidy(pCounter);
      pSubsidy.pGel := pSubsidy.pGel * (1 - NVL(pSubsidy.pCutDays, 0)/(pSubsidy.pEnd - pSubsidy.pStart + 1));
      pResult.pSubsidy(pCounter) := pSubsidy;
      pCounter := pCounter + 1;
    END LOOP;
  END IF;

END applyCuts;

/**
 * Calculate charge for a given customer and month using ProcedureA. In the
 * previous version we needed also parameter which obliged us to determine
 * whether previous charge existed or not. But we do not need it now, because
 * ProcedureA is now used only for FULL charge calculation for a given month.
 *
 * @param pCustomerId Id of the customer for which we need to calculate charge
 * @param pMonth calculation month
 * @param pHint calculation hint
 * @outparam pResults calculation result
 */
PROCEDURE calcMonthA (
  pCustomerId  NUMBER,
  pMonth       TP_MONTH,
  pHint        NUMBER,
  pResults OUT TP_CALC_RES
) IS
  pDetailUsed BOOLEAN;
  pDetailRow bs.trash_cust_det%ROWTYPE;
  pDetailCount NUMBER := 0;
  pDetailFound BOOLEAN;
  pLastDetailAmount NUMBER;
  pLastCatUnitId NUMBER;
  pGel NUMBER;
  pCoeffDetailId NUMBER;
  pChargeDetail TP_CHARGE_DETAIL;
  pSubsidyDetailRow BS.TRASHSUBSIDIES%ROWTYPE;
  pPersonCount NUMBER;
  pSubsidyGel NUMBER;
  pSubsidyCoeff NUMBER;
  pSubsidyDetail TP_SUBSIDY_DETAIL;
BEGIN

  -- Loop over all categories in appropriate interval
  FOR rec IN (
    SELECT DISTINCT cat_unit_id
    FROM bs.trash_cust_det
    WHERE ((start_date <= pMonth.pDetailFirst AND (end_date IS NULL OR end_date >= pMonth.pDetailFirst))
        OR (start_date <= pMonth.pDetailLast AND (end_date IS NULL OR end_date >= pMonth.pDetailLast)))
      AND (
        (pHint = HINT_DEFAULT    AND status = 0) OR
        (pHint = HINT_AS_VOUCHER /*any status when calculating as voucher*/)
      ) AND customer_id = pCustomerId
  ) LOOP

    -- Getting customer detail row for this category unit.
    BEGIN
      SELECT * INTO pDetailRow FROM (
        SELECT det.* FROM bs.trash_cust_det det, bs.trash_recalc_cust_det recalc_det
        WHERE ((start_date <= pMonth.pDetailFirst AND (end_date IS NULL OR end_date >= pMonth.pDetailFirst))
            OR (start_date <= pMonth.pDetailLast AND (end_date IS NULL OR end_date >= pMonth.pDetailLast)))
          AND det.customer_id = pCustomerId
          AND det.cat_unit_id = rec.cat_unit_id
          AND det.id = recalc_det.cust_det_id (+)
          AND (
            -- normal hint
            (pHint = HINT_DEFAULT) OR
            -- voucher hint: we should check whether charge exists
            (pHint = HINT_AS_VOUCHER AND (ignore_this_detail IS NULL OR ignore_this_detail = 0))
        ) ORDER BY
          -- This was the old assumption:
          -- ID DESC
          ----------------------------------------------------------------------
          -- And this is the new one: priority here is first for active detail,
          -- then for closed and at last for inactive. For overlapping detail
          -- periods a new detail has more weight then a later one (we determine
          -- oldness by detail record ID)
          ----------------------------------------------------------------------
          CASE
            WHEN det.STATUS = 0 THEN 1 -- active
            WHEN det.STATUS = 1 THEN 3 -- inactive
            WHEN det.STATUS = 2 THEN 2 -- closed
            ELSE 10 -- unknown status
          END ASC,
          det.ID  DESC
      ) WHERE ROWNUM = 1;
      pDetailFound := TRUE;
    EXCEPTION WHEN NO_DATA_FOUND THEN
      pDetailFound := FALSE;
    END;

    -- when detail found, calculate it!
    IF pDetailFound
    THEN
      -- check for categories: at this place old procedure compares tariffIds,
      -- but tariffId is now deprecated parameter, so we need to check using
      -- simply Category/Unit mapping ids.
      IF pLastCatUnitId IS NOT NULL AND pLastCatUnitId = CUM_RESIDENT_JAN2007
        AND pDetailRow.CAT_UNIT_ID != CUM_RESIDENT_JAN2007
      THEN
        RAISE_APPLICATION_ERROR(-20000, 'Mixture of Resident and Comercial '
          || 'categories. Can not calculate any more.');
      END IF;

      -- calculate using coefficient: it will be only one
      calcByCoeffDetailsA(pDetailRow.CAT_UNIT_ID, pDetailRow.AMOUNT, pMonth,
        pGel, pCoeffDetailId);

      -- create charge detail
      pChargeDetail.pCoeffId := pCoeffDetailId;
      pChargeDetail.pCustomerDetailId := pDetailRow.ID;
      pChargeDetail.pGel := pGel;
      pChargeDetail.pCustomerDetailCatUnitId := pDetailRow.cat_unit_id;
      pChargeDetail.pStart := pMonth.pFirst;
      pChargeDetail.pEnd := pMonth.pLast;
      pChargeDetail.pCustomerDetailAmount := pDetailRow.amount;
      -- add this detail to final calculation results
      appendCharge(pChargeDetail, pResults);

      -- change control parameters
      pDetailCount := pDetailCount + 1;
      pLastDetailAmount := pDetailRow.AMOUNT;
      pLastCatUnitId := pDetailRow.CAT_UNIT_ID;
    END IF;

    -- Now check whether subsidy can be calculated. This check also includes
    -- whether we have enough GEL charge.
    IF pDetailCount = 1 AND pLastCatUnitId = CUM_RESIDENT_JAN2007
      AND ABS(pGel) >= MIN_GEL
    THEN
    
      BEGIN
        -- getting subsidy detail
        SELECT * INTO pSubsidyDetailRow FROM (SELECT * FROM BS.TRASHSUBSIDIES  WHERE
          ((fromdate <= pMonth.pDetailFirst AND (todate IS NULL OR todate >= pMonth.pDetailFirst))
            OR (fromdate <= pMonth.pDetailLast AND (todate IS NULL OR todate >= pMonth.pDetailLast)))
          AND custkey = pCustomerId
        ORDER BY
          -- active details have more privelegies
          status ASC,
          trashsubsidyid DESC)
        WHERE ROWNUM = 1;

      EXCEPTION WHEN NO_DATA_FOUND
      THEN
        NULL;
      END;

      -- getting persons count and adjusting it
      pPersonCount := NVL(pLastDetailAmount, 0);
      IF pPersonCount > NVL(pSubsidyDetailRow.quantity, 0)
      THEN
        pPersonCount := NVL(pSubsidyDetailRow.quantity, 0);
      END IF;

      -- only when PersonCount > 0 we can continue:
      IF pPersonCount > 0
      THEN
        -- calculate subsidy GEL charge
        pSubsidyCoeff := pPersonCount / pLastDetailAmount;
        pSubsidyGel := 0;
        IF pSubsidyDetailRow.OPERATIONID = OPER_SUBSIDY_50_PERCENT
        THEN
          pSubsidyGel := -0.5 * pGel * pSubsidyCoeff;
        ELSIF pSubsidyDetailRow.OPERATIONID = OPER_SUBSIDY_100_PERCENT
        THEN
          pSubsidyGel := -1.0 * pGel * pSubsidyCoeff;
        ELSE
          RAISE_APPLICATION_ERROR(-20000, 'Not supported subsidy operation '
            || pSubsidyDetailRow.OPERATIONID || '.');
        END IF;

        -- adding detail
        IF ABS(pSubsidyGel) > MIN_GEL
        THEN
          -- create detail
          pSubsidyDetail.pSubsidyDetailId := pSubsidyDetailRow.trashsubsidyid;
          pSubsidyDetail.pCustomerDetailId := pDetailRow.ID;
          pSubsidyDetail.pPersonCount := pPersonCount;
          pSubsidyDetail.pGel := pSubsidyGel;
          pSubsidyDetail.pOperationId := pSubsidyDetailRow.operationid;
          pSubsidyDetail.pStart := pMonth.pFirst;
          pSubsidyDetail.pEnd := pMonth.pLast;
          -- append detail into results
          appendSubsidy(pSubsidyDetail, pResults);
        END IF;
      END IF;
    END IF;

  END LOOP;

  -- apply cuts
  applyCuts(pCustomerId, pMonth, pResults);

END calcMonthA;

/**
 * Calculate charge for a given customer and month using ProcedureB.
 */
PROCEDURE calcMonthB (
  pCustomerId  NUMBER,
  pMonth       TP_MONTH,
  pHint        NUMBER,
  pResults OUT TP_CALC_RES
) IS
  pNewStartDate DATE;
  pNewEndDate DATE;
  pCharges TP_CHARGE_DETAILS;
  pLastEndDate DATE;
  pD1 DATE;
  pD2 DATE;
  pContinue BOOLEAN;
  pCounter NUMBER;
  pCharge TP_CHARGE_DETAIL;
  pSubsidies TP_SUBSIDY_DETAILS;
BEGIN

  -- TODO: we need to add checking on intersection of residential/comercial categories

  -- Loop over all categories in appropriate interval
  FOR rec IN (
    SELECT DISTINCT cat_unit_id
    FROM bs.trash_cust_det
    WHERE ((start_date <= pMonth.pFirst AND (end_date IS NULL OR end_date >= pMonth.pFirst))
        OR (start_date <= pMonth.pLast AND (end_date IS NULL OR end_date >= pMonth.pLast)))
      AND (
        (pHint = HINT_DEFAULT AND status = 0) OR
        (pHint = HINT_AS_VOUCHER /*any status is OK, when calculating as voucher*/)
      ) AND customer_id = pCustomerId
  ) LOOP

    -- start/end dates = this month
    pNewStartDate := pMonth.pFirst;
    pNewEndDate := pMonth.pLast;
    pLastEndDate := pNewStartDate;

    -- loop over this detail
    FOR det IN (
      SELECT det.* FROM bs.trash_cust_det det, bs.trash_recalc_cust_det recalc_det
      WHERE ((start_date <= pMonth.pFirst AND (end_date IS NULL OR end_date >= pMonth.pFirst))
        OR (start_date <= pMonth.pLast AND (end_date IS NULL OR end_date >= pMonth.pLast)))
        AND det.customer_id = pCustomerId -- this customer
        AND det.cat_unit_id = rec.cat_unit_id -- this detail
        AND det.id = recalc_det.cust_det_id (+)
        AND (
          -- normal hint
          (pHint = HINT_DEFAULT) OR
          -- voucher hint
          (pHint = HINT_AS_VOUCHER AND (ignore_this_detail IS NULL OR ignore_this_detail = 0))
        )
      ORDER BY
        /*CASE
          WHEN det.STATUS = 0 THEN 1 -- active
          WHEN det.STATUS = 1 THEN 3 -- inactive
          WHEN det.STATUS = 2 THEN 2 -- closed
          ELSE 10 -- unknown status
        END ASC,*/
        det.ID ASC
    ) LOOP

      -- check interval
      IF pLastEndDate <= pNewEndDate
      THEN

        -- find intersection (pD1, pD2)
        findIntersection(pLastEndDate, pNewEndDate, det.start_date, det.end_date, pContinue, pD1, pD2);

        -- when interval is correct
        IF pContinue AND TRUNC(pD1) <= TRUNC(pD2)
        THEN

          -- calculate charge for given interval
          pCharges := NULL;
          calcByCoeffDetailsB(det.cat_unit_id, det.amount, det.id, det.cat_unit_id, pD1, pD2, pMonth, pCharges);

          -- append charge
          appendCharges(pCharges, pResults);

          -- calculate subsidy (for resident category only!)
          IF pCharges IS NOT NULL AND rec.cat_unit_id = CUM_RESIDENT_JAN2007
          THEN
            -- initialize counter
            pCounter := 1;
            WHILE pCounter <= pCharges.COUNT
            LOOP
              -- getting charge
              pCharge := pCharges(pCounter);

              -- calculate appropriate subsidy
              pSubsidies := NULL;
              calcSubsidyB_2009(det.CUSTOMER_ID, pCharge, pSubsidies);
              
              -- append subsidy
              IF pSubsidies IS NOT NULL
              THEN
                appendSubsidy(pSubsidies, pResults);
              END IF;

              -- increase counter
              pCounter := pCounter + 1;
            END LOOP;

          END IF;
        END IF;

        -- last end date
        pLastEndDate := pD2 + 1;
      END IF;
    END LOOP;
  END LOOP;

  -- apply cuts
  applyCuts(pCustomerId, pMonth, pResults);

END calcMonthB;

/**
 * Calculate month.
 */
PROCEDURE calcMonth (
  pCustomerId  NUMBER,
  pMonth       TP_MONTH,
  pHint        NUMBER,
  pResults OUT TP_CALC_RES
) IS
  pProcedureName CHAR(1);
BEGIN

  -- getting name of the procedure which should be used for the given month
  pProcedureName := getProcedureName(pMonth, pHint);
  
  -- ProcedureA
  IF pProcedureName = NAME_PROCEDURE_A
  THEN
    calcMonthA(pCustomerId, pMonth, pHint, pResults);
  -- ProcedureB
  ELSIF pProcedureName = NAME_PROCEDURE_B
  THEN
    calcMonthB(pCustomerId, pMonth, pHint, pResults);
  -- Unknown procedure name
  ELSE
    RAISE_APPLICATION_ERROR(-20000, 'Unknown procedure name "' || pProcedureName || '"');
  END IF;

END calcMonth;

/**
 * Use this procedure for consequent months charging.
 */
PROCEDURE runRegularTrash (
  pMonthDate DATE,
  pItemDate  DATE
) IS
  pMonth TP_MONTH;
  pResults TP_CALC_RES;
  pCounter NUMBER;
  pSummaryGel NUMBER;
  pNewItemId NUMBER;
  pNewSubsidyItemId NUMBER;
  pCharge TP_CHARGE_DETAIL;
  pSubsidy TP_SUBSIDY_DETAIL;
BEGIN

  -- check month and item dates
  IF pMonthDate IS NULL OR pItemDate IS NULL
  THEN
    raise_application_error (-20000, 'Not defined operation and/or item dates.');
  END IF;

  -- getting calculation month
  getMonth(pMonthDate, pMonth);

  -- loop over all active customers: current situation
  FOR rec IN (SELECT * FROM bs.trashcustomer WHERE (status IS NULL OR status = 0))
  LOOP

    -- #1. calculate this month
    calcMonth(rec.CUSTKEY, pMonth, HINT_DEFAULT, pResults);

    -- #2. append charge
    IF pResults.pCharge IS NOT NULL AND pResults.pCharge.COUNT > 0
    THEN
      -- add header item
      INSERT INTO bs.trashitem (operdate, enterdate, amount, custkey, operationid, isprinted)
      VALUES (TRUNC(pItemDate), SYSDATE, 0, rec.custkey, OPER_CHARGE, 0)
      RETURNING trashitemid INTO pNewItemId;

      -- adding details for the item
      pCounter := 1;
      pSummaryGel := 0;
      WHILE pCounter <= pResults.pCharge.COUNT
      LOOP
        -- getting charge
        pCharge := pResults.pCharge(pCounter);

        -- inserting charge
        --IF ABS(pCharge.pGel) >= MIN_GEL
        --THEN
          -- add detail record
          INSERT INTO bs.trash_item_det (item_id, cat_coeff_id, start_date, end_date,
            amnt_in_m3, amnt_in_gel, enter_date, cust_det_id)
          VALUES (pNewItemId, pCharge.pCoeffId, pCharge.pStart, pCharge.pEnd,
            0, ROUND (pCharge.pGel, 2), SYSDATE, pCharge.pCustomerDetailId);
          -- make summary
          pSummaryGel := pSummaryGel + ROUND(pCharge.pGel, 2);
        --END IF;

        -- next step
        pCounter := pCounter + 1;
      END LOOP;

      -- update header and customer balance
      IF ABS(pSummaryGel) >= MIN_GEL
      THEN
        UPDATE bs.trashitem
          SET amount = ROUND(pSummaryGel, 2)
        WHERE trashitemid = pNewItemId;

        UPDATE bs.trashcustomer
          SET balance = balance + ROUND(pSummaryGel, 2),
            curr_balance = curr_balance + ROUND(pSummaryGel, 2)
        WHERE custkey = rec.custkey;
      END IF;

    END IF;

    -- #3. append subsidy
    IF pResults.pSubsidy IS NOT NULL AND pResults.pSubsidy.COUNT > 0
    THEN
      -- add subsidy header item
      INSERT INTO bs.trashitem (operdate, enterdate, amount, trashtariffvalueid, custkey, operationid, isprinted)
      VALUES (TRUNC(pItemDate), SYSDATE, 0, 0, rec.custkey, pResults.pSubsidy(1).pOperationId, 0)
      RETURNING trashitemid INTO pNewSubsidyItemId;

      -- adding details for the item
      pCounter := 1;
      pSummaryGel := 0;
      WHILE pCounter <= presults.pSubsidy.COUNT
      LOOP
        -- getting subsidy
        pSubsidy := pResults.pSubsidy(pCounter);

        -- inserting subsidy detail
        IF ABS(pSubsidy.pGel) >= MIN_GEL
        THEN
          -- insert into item details
          INSERT INTO bs.trash_item_det (item_id, start_date, end_date,
            amnt_in_m3, amnt_in_gel, enter_date, parent_item_id,
            subs_det_id, subs_count, cust_det_id)
          VALUES (pNewSubsidyItemId, pMonth.pFirst, pMonth.pLast,
            0, pSubsidy.pGel, SYSDATE, pNewItemId,
            pSubsidy.pSubsidyDetailId, pSubsidy.pPersonCount, pSubsidy.pCustomerDetailId
          );
          pSummaryGel := pSummaryGel + ROUND(pSubsidy.pGel, 2);
        END IF;

        -- next step
        pCounter := pCounter + 1;
      END LOOP;

      -- update header and customer balance
      IF ABS(pSummaryGel) >= MIN_GEL
      THEN
        UPDATE bs.trashitem
          SET amount = ROUND(pSummaryGel, 2)
        WHERE trashitemid = pNewSubsidyItemId;

        UPDATE bs.trashcustomer
          SET balance = balance + ROUND(pSummaryGel, 2),
            curr_balance = curr_balance + ROUND(pSummaryGel, 2)
        WHERE custkey = rec.custkey;
      -- delete header item, because there is no charge
      ELSE
        DELETE bs.trashitem
        WHERE trashitemid = pNewSubsidyItemId;
      END IF;

    END IF;

  END LOOP;

END runRegularTrash;

----------------------------- Correction Utilities -----------------------------

PROCEDURE ensure_old_balance_ge_0 (p_customer_id NUMBER)
IS
  l_curr_old_balance NUMBER;
BEGIN

  -- get old balance value for the customer
  SELECT OLD_BALANCE INTO l_curr_old_balance
  FROM bs.trashcustomer
  WHERE custkey = p_customer_id;

  IF l_curr_old_balance < 0
  THEN
    IF ABS(l_curr_old_balance) < MIN_GEL
    THEN
      UPDATE bs.trashcustomer
      SET OLD_BALANCE = 0
      WHERE custkey = p_customer_id;
    ELSE
      INSERT INTO BS.TRASHITEM (
        OPERDATE,       ENTERDATE, AMOUNT,CUSTKEY,    OPERATIONID
      ) VALUES(
        TRUNC(sysdate), sysdate,   0,  p_customer_id, OLD_BALANCE_GE_0_OPERATION
      );
    END IF;
  END IF;

END;

/**
 * Asserts correction interval.
 */
PROCEDURE assertCorrectionInterval(pStart DATE, pEnd DATE) IS
BEGIN

  -- if start date > end date
  IF pStart > pEnd
  THEN
    RAISE_APPLICATION_ERROR(-20000, 'Start date is greater than end date.');
  END IF;

  -- Start date is out of range
  IF NOT pStart BETWEEN MIN_CORRECTION_DATE AND MAX_CORRECTION_DATE
  THEN
    RAISE_APPLICATION_ERROR(-20000, 'Start date is out of supported interval.');
  END IF;

  -- End date is out of range
  IF NOT pEnd BETWEEN MIN_CORRECTION_DATE AND MAX_CORRECTION_DATE
  THEN
    RAISE_APPLICATION_ERROR(-20000, 'End date is out of supported interval.');
  END IF;

END; -- assert_correction_interval

/**
 * Calculates existing charge and subsidies.
 */
PROCEDURE calculateExistingCharge(
  pCustomerId     NUMBER,
  pMonth          TP_MONTH,
  pChargeGel  OUT NUMBER,
  pSubsidyGel OUT NUMBER
) IS
BEGIN

  -- getting existing charge
  SELECT SUM(AMNT_IN_GEL) INTO pChargeGel
  FROM bs.TRASHITEM it, bs.TRASH_ITEM_DET det
  WHERE CUSTKEY = pCustomerId
    AND det.ITEM_ID = it.TRASHITEMID
    AND det.START_DATE BETWEEN pMonth.pFirst AND pMonth.pLast
    AND det.END_DATE BETWEEN pMonth.pFirst AND pMonth.pLast
    AND it.OPERATIONID IN (
      OPER_CHARGE,
      OPER_PREV_PERIOD_CHARGE,
      OPER_CHARGE_CORRECTION,
      OPER_CHARGE_CORR_AFTER_2011
  );

  -- getting existing subsidy
  SELECT SUM(AMNT_IN_GEL) INTO pSubsidyGel
  FROM bs.TRASHITEM it, bs.TRASH_ITEM_DET det
  WHERE CUSTKEY = pCustomerId
    AND det.ITEM_ID = it.TRASHITEMID
    AND det.START_DATE BETWEEN pMonth.pFirst AND pMonth.pLast
    AND det.END_DATE BETWEEN pMonth.pFirst AND pMonth.pLast
    AND it.OPERATIONID IN (
      OPER_SUBSIDY_50_PERCENT,
      OPER_SUBSIDY_100_PERCENT,
      OPER_PREV_PERIOD_SUBSIDY,
      OPER_SUBSIDY_CORRECTION,
      OPER_SUBSIDY_CORR_AFTER_2011
  );

  -- slight adjusting, in case if we have NULL instead of number
  pChargeGel := NVL(pChargeGel, 0);
  pSubsidyGel := NVL(pSubsidyGel, 0);

END calculateExistingCharge;

/**
 * Recalculates given month summaries: charge and subsidy GEL amounts.
 */
PROCEDURE recalculateMonth(
  pCustomerId     NUMBER,
  pMonth          TP_MONTH,
  pChargeGel  OUT NUMBER,
  pSubsidyGel OUT NUMBER
) IS
  pResults TP_CALC_RES;
  pCounter NUMBER;
BEGIN

  -- calculate month
  calcMonth(pCustomerId, pMonth, HINT_AS_VOUCHER, pResults);

  -- initial values
  pChargeGel := 0;
  pSubsidyGel := 0;

  -- summarize charges
  IF pResults.pCharge IS NOT NULL AND pResults.pCharge.COUNT > 0
  THEN
    pCounter := 1;
    WHILE pCounter <= pResults.pCharge.COUNT
    LOOP
      pChargeGel := pChargeGel + ROUND(pResults.pCharge(pCounter).pGel, 2);
      pCounter := pCounter + 1;
    END LOOP;
  END IF;

  -- summarize subsidies
  IF pResults.pSubsidy IS NOT NULL AND pResults.pSubsidy.COUNT > 0
  THEN
    pCounter := 1;
    WHILE pCounter <= pResults.pSubsidy.COUNT
    LOOP
      pSubsidyGel := pSubsidyGel + ROUND(pResults.pSubsidy(pCounter).pGel, 2);
      pCounter := pCounter + 1;
    END LOOP;
  END IF;

END recalculateMonth;

/**
 * Main recalculation routine. Both recalculate and recalculateAndSendToItem
 * use it with different hint options.
 *
 * 0 - only recalculate (fill recalculation tables)
 * 1 - recalculate and send to item
 */
PROCEDURE recalculationRoutine(
  pCustomerId NUMBER,
  pHint       NUMBER,
  pStart      DATE := MIN_CORRECTION_DATE,
  pEnd        DATE := MAX_CORRECTION_DATE
) IS
  pNewHint NUMBER := NVL(pHint, 0);
  pNewStart DATE;
  pNewEnd   DATE;
  pDate     DATE;
  pRecalcCharge  NUMBER;
  pRecalcSubsidy NUMBER;
  pExistCharge   NUMBER;
  pExistSubsidy  NUMBER;
  pMonth TP_MONTH;
  pNewItemId NUMBER;
  pNewSubsidyItem NUMBER;
  pNewItemId_2011 NUMBER;
  pNewSubsidyItem_2011 NUMBER;
  pSummaryCharge NUMBER;
  pSummarySubsidy NUMBER;
  pSummaryCharge_2011 NUMBER;
  pSummarySubsidy_2011 NUMBER;
  pChargeDetail BOOLEAN;
  pSubsidyDetail BOOLEAN;
  pChargeDetail_2011 BOOLEAN;
  pSubsidyDetail_2011 BOOLEAN;
  l_itemid NUMBER;
  l_operid NUMBER;
  l_curr_balance NUMBER;
  l_curr_old_balance NUMBER;
  l_curr_tech_balance NUMBER;
  l_next_balance NUMBER;
  l_next_old_balance NUMBER;
  l_next_tech_balance NUMBER;
BEGIN

  -- assert interval
  assertCorrectionInterval(pStart, pEnd);

  -- clear previous calculation results for this customer
  DELETE FROM BS.TRASH_RECALC_RESULTS WHERE CUSTOMER_ID = pCustomerId;

  -- define new interval: first days of the months
  pNewStart := TRUNC(LAST_DAY(ADD_MONTHS(pStart, -1))) + 1;
  pNewEnd := TRUNC(LAST_DAY(ADD_MONTHS(pEnd, -1))) + 1;

  -- initialize current date
  pDate := pNewStart;

  -- create header item when appropriate hint
  IF pHint != 0
  THEN
    -- charge headers
    INSERT INTO bs.trashitem (operdate, enterdate, amount, custkey, operationid, isprinted)
    VALUES (TRUNC(SYSDATE), SYSDATE, 0, pCustomerId, OPER_CHARGE_CORRECTION, 0)
    RETURNING trashitemid INTO pNewItemId;
    INSERT INTO bs.trashitem (operdate, enterdate, amount, custkey, operationid, isprinted)
    VALUES (TRUNC(SYSDATE), SYSDATE, 0, pCustomerId, OPER_CHARGE_CORR_AFTER_2011, 0)
    RETURNING trashitemid INTO pNewItemId_2011;

    -- subsidy headers
    INSERT INTO bs.trashitem (operdate, enterdate, amount, custkey, operationid, isprinted)
    VALUES (TRUNC(SYSDATE), SYSDATE, 0, pCustomerId, OPER_SUBSIDY_CORRECTION, 0)
    RETURNING trashitemid INTO pNewSubsidyItem;
    INSERT INTO bs.trashitem (operdate, enterdate, amount, custkey, operationid, isprinted)
    VALUES (TRUNC(SYSDATE), SYSDATE, 0, pCustomerId, OPER_SUBSIDY_CORR_AFTER_2011, 0)
    RETURNING trashitemid INTO pNewSubsidyItem_2011;

    -- initialize meta data parameters
    pSummaryCharge := 0;
    pSummarySubsidy := 0;
    pSummaryCharge_2011 := 0;
    pSummarySubsidy_2011 := 0;
    pChargeDetail := FALSE;
    pSubsidyDetail := FALSE;
    pChargeDetail_2011 := FALSE;
    pSubsidyDetail_2011 := FALSE;
  END IF;

  -- loop over each month in the interval
  WHILE pDate BETWEEN pNewStart AND pNewEnd
  LOOP
    -- getting current month
    getMonth(pdate, pMonth);

    -- existing charge and subsidy
    calculateExistingCharge(pCustomerId, pMonth, pExistCharge, pExistSubsidy);

    -- recalculated charge and subsidy
    recalculateMonth(pCustomerId, pMonth, pRecalcCharge, pRecalcSubsidy);

    ----------------------------
    -- make charge correction --
    ----------------------------

    IF ABS(pRecalcCharge - pExistCharge) >= MIN_GEL
    THEN

      -- before/after 2011 year
      IF pDate < CORR_START_DATE_2011
      THEN
        pChargeDetail := TRUE;
        l_operid := OPER_CHARGE_CORRECTION;
        l_itemid := pNewItemId;
        pSummaryCharge := pSummaryCharge - pExistCharge + pRecalcCharge;
      ELSE
        pChargeDetail_2011 := TRUE;
        l_operid := OPER_CHARGE_CORR_AFTER_2011;
        l_itemid := pNewItemId_2011;
        pSummaryCharge_2011 := pSummaryCharge_2011 - pExistCharge + pRecalcCharge;
      END IF;

      -- add to Recalculation Results table
      IF pNewHint = 0
      THEN

          IF ABS(pExistCharge) >= MIN_GEL
          THEN
            -- discharge
            INSERT INTO BS.TRASH_RECALC_RESULTS (
              customer_id, operation_id, start_date, end_date, corr_gel, corr_m3
            ) VALUES (
              pCustomerId, l_operid, pMonth.pFirst, pMonth.pLast, -pExistCharge, 0
            );
          END IF;

          IF ABS(pRecalcCharge) >= MIN_GEL
          THEN
            -- recharge
            INSERT INTO BS.TRASH_RECALC_RESULTS (
              customer_id, operation_id, start_date, end_date, corr_gel, corr_m3
            ) VALUES (
              pCustomerId, l_operid, pMonth.pFirst, pMonth.pLast, pRecalcCharge, 0
            );
          END IF;

      -- add to TrashItem table
      ELSE

          -- add DISCHARGE detail record
          IF ABS(pExistCharge) >= MIN_GEL
          THEN
            INSERT INTO bs.trash_item_det (
              item_id, start_date, end_date, amnt_in_m3, amnt_in_gel, enter_date
            ) VALUES (
              l_itemid, pMonth.pFirst, pMonth.pLast, 0, -pExistCharge, SYSDATE
            );
          END IF;

          -- add RECHARGE detail record
          IF ABS(pRecalcCharge) >= MIN_GEL
          THEN
            INSERT INTO bs.trash_item_det (
              item_id, start_date, end_date, amnt_in_m3, amnt_in_gel, enter_date
            ) VALUES (
              l_itemid, pMonth.pFirst, pMonth.pLast, 0, pRecalcCharge, SYSDATE
            );
          END IF;

      END IF;

    END IF;

    -----------------------------
    -- make subsidy correction --
    -----------------------------
    IF ABS(pRecalcSubsidy - pExistSubsidy) >= MIN_GEL
    THEN
    
      -- before/after 2011 year
      IF pDate < CORR_START_DATE_2011
      THEN
        pSubsidyDetail := TRUE;
        l_operid := OPER_SUBSIDY_CORRECTION;
        l_itemid := pNewSubsidyItem;
        pSummarySubsidy := pSummarySubsidy - pExistSubsidy + pRecalcSubsidy;
      ELSE
        pSubsidyDetail_2011 := TRUE;
        l_operid := OPER_SUBSIDY_CORR_AFTER_2011;
        l_itemid := pNewSubsidyItem_2011;
        pSummarySubsidy_2011 := pSummarySubsidy_2011 - pExistSubsidy + pRecalcSubsidy;
      END IF;

      -- add to Recalculation Results table
      IF pNewHint = 0
      THEN

          IF ABS(pExistSubsidy) >= MIN_GEL
          THEN
            -- discharge
            INSERT INTO BS.TRASH_RECALC_RESULTS (
              customer_id, operation_id, start_date, end_date, corr_gel, corr_m3
            ) VALUES (
              pCustomerId, l_operid, pMonth.pFirst, pMonth.pLast, -pExistSubsidy, 0
            );
          END IF;

          IF ABS(pRecalcSubsidy) >= MIN_GEL
          THEN
            -- recharge
            INSERT INTO BS.TRASH_RECALC_RESULTS (
              customer_id, operation_id, start_date, end_date, corr_gel, corr_m3
            ) VALUES (
              pCustomerId, l_operid, pMonth.pFirst, pMonth.pLast, pRecalcSubsidy, 0
            );
          END IF;

      -- add to TrashItem table
      ELSE

          -- add DISCHARGE detail record
          IF ABS(pExistSubsidy) >= MIN_GEL
          THEN
            INSERT INTO bs.trash_item_det (
              item_id, start_date, end_date, amnt_in_m3, amnt_in_gel, enter_date
            ) VALUES (
              l_itemid, pMonth.pFirst, pMonth.pLast, 0, -pExistSubsidy, SYSDATE
            );
          END IF;

          -- add RECHARGE detail record
          IF ABS(pRecalcSubsidy) >= MIN_GEL
          THEN
            INSERT INTO bs.trash_item_det (
              item_id, start_date, end_date, amnt_in_m3, amnt_in_gel, enter_date
            ) VALUES (
              l_itemid, pMonth.pFirst, pMonth.pLast, 0, pRecalcSubsidy, SYSDATE
            );
          END IF;

      END IF;
    END IF;


    -- next month!
    pDate := TRUNC(ADD_MONTHS (pDate, 1));
  END LOOP;

  -- update balances
  IF pHint != 0
  THEN

    SELECT 
      BALANCE, OLD_BALANCE, TECH_BALANCE
    INTO
      l_curr_balance, l_curr_old_balance, l_curr_tech_balance
    FROM
      BS.TRASHCUSTOMER
    WHERE
      CUSTKEY = pCustomerId;

    l_next_balance      := l_curr_balance;
    l_next_old_balance  := l_curr_old_balance;
    l_next_tech_balance := l_curr_tech_balance;

    -- manage charges (before 2011)
    IF NOT pChargeDetail
    THEN
      -- delete item without any charge
      DELETE FROM BS.TRASHITEM WHERE TRASHITEMID = pNewItemId;
    ELSE
      -- update item amount
      UPDATE
        BS.TRASHITEM
      SET
        AMOUNT = pSummaryCharge,
        BALANCE      = l_next_balance,
        OLD_BALANCE  = l_next_old_balance,
        TECH_BALANCE = l_next_tech_balance
      WHERE
        TRASHITEMID = pNewItemId;

      -- next balances
      l_next_balance      := l_next_balance      + pSummaryCharge;
      l_next_old_balance  := l_next_old_balance  + pSummaryCharge;
      l_next_tech_balance := l_next_tech_balance + pSummaryCharge;
    END IF;

    -- manage charges (after 2011)
    IF NOT pChargeDetail_2011
    THEN
      -- delete item without any charge
      DELETE FROM BS.TRASHITEM WHERE TRASHITEMID = pNewItemId_2011;
    ELSE
      -- update item amount
      UPDATE
        BS.TRASHITEM
      SET
        AMOUNT = pSummaryCharge_2011,
        BALANCE = l_next_balance,
        OLD_BALANCE = l_next_old_balance,
        TECH_BALANCE = l_next_tech_balance
      WHERE
        TRASHITEMID = pNewItemId_2011;

      -- next balances
      l_next_balance      := l_next_balance      + pSummaryCharge_2011;
      l_next_old_balance  := l_next_old_balance  + 0; -- no effect on old charge!!!
      l_next_tech_balance := l_next_tech_balance + 0; -- no effect on old charge!!!
    END IF;

    -- manage subsidies (before 2011)
    IF NOT pSubsidyDetail
    THEN
      -- delete item without any charge
      DELETE FROM BS.TRASHITEM WHERE TRASHITEMID = pNewSubsidyItem;
    ELSE
      -- update item amount
      UPDATE
        BS.TRASHITEM
      SET
        AMOUNT = pSummarySubsidy,
        BALANCE = l_next_balance,
        OLD_BALANCE = l_next_old_balance,
        TECH_BALANCE = l_next_tech_balance
      WHERE
        TRASHITEMID = pNewSubsidyItem;

      -- next balances
      l_next_balance      := l_next_balance      + pSummarySubsidy;
      l_next_old_balance  := l_next_old_balance  + pSummarySubsidy;
      l_next_tech_balance := l_next_tech_balance + pSummarySubsidy;
    END IF;

    -- manage subsidies (after 2011)
    IF NOT pSubsidyDetail_2011
    THEN
      -- delete item without any charge
      DELETE FROM BS.TRASHITEM WHERE TRASHITEMID = pNewSubsidyItem_2011;
    ELSE
      -- update item amount
      UPDATE
        BS.TRASHITEM
      SET
        AMOUNT = pSummarySubsidy_2011,
        BALANCE = l_next_balance,
        OLD_BALANCE = l_next_old_balance,
        TECH_BALANCE = l_next_tech_balance
      WHERE
        TRASHITEMID = pNewSubsidyItem_2011;

      -- next balances
      l_next_balance      := l_next_balance      + pSummarySubsidy_2011;
      l_next_old_balance  := l_next_old_balance  + 0; -- no effect on old charge!!!
      l_next_tech_balance := l_next_tech_balance + 0; -- no effect on old charge!!!
    END IF;

    -- Update trashcustomer
    UPDATE BS.TRASHCUSTOMER
    SET
      BALANCE      = l_next_balance,
      OLD_BALANCE  = l_next_old_balance,
      TECH_BALANCE = l_next_tech_balance,
      CURR_BALANCE = l_next_balance - l_next_old_balance
    WHERE CUSTKEY  = pCustomerId;

    -- ensure old_balance >= 0
    ensure_old_balance_ge_0(pCustomerId);
  END IF;

END recalculationRoutine;

/**
 * Recalculation (difference between actual and newly calculated charges) for
 * the full history with monthly details.
 */
PROCEDURE recalculate(
  pCustomerId NUMBER,
  pStart       DATE := MIN_CORRECTION_DATE,
  pEnd         DATE := MAX_CORRECTION_DATE
) IS
BEGIN
  -- recalculate with hint "0"
  recalculationRoutine(pCustomerId, 0, pStart, pEnd);
END recalculate;

/**
 * Recalculates customer and sends results to item.
 */
PROCEDURE recalculateAndSendToItem(
  pCustomerId NUMBER,
  pStart      DATE := MIN_CORRECTION_DATE,
  pEnd        DATE := MAX_CORRECTION_DATE
) IS
BEGIN
  -- recalculate with hint "0"
  recalculationRoutine(pCustomerId, 1, pStart, pEnd);
END recalculateAndSendToItem;

/**
 * Updates customer detail ID.
 * 
 * pDisable = 0 -- activate
 * pDisable = 1 -- disable
 */
PROCEDURE editRecalculationDetail(pCustomerDetId NUMBER, pDisable NUMBER) IS
  pNewDisable NUMBER;
  pId NUMBER;
BEGIN

  -- adjust disable flag
  IF NVL(pDisable, 0) = 0
  THEN
    pNewDisable := 0;
  ELSE
    pNewDisable := 1;
  END IF;

  -- get ID of the detail
  BEGIN
    SELECT ID INTO pId FROM BS.TRASH_RECALC_CUST_DET
    WHERE cust_det_id = pCustomerDetId;
  EXCEPTION WHEN NO_DATA_FOUND
  THEN
    INSERT INTO BS.TRASH_RECALC_CUST_DET (cust_det_id, ignore_this_detail)
    VALUES (pCustomerDetId, pNewDisable);
  END;

  -- update recalculation detail
  IF pId IS NOT NULL
  THEN
    -- update detail
    UPDATE BS.TRASH_RECALC_CUST_DET
      SET IGNORE_THIS_DETAIL = pNewDisable
    WHERE
      ID = pId;
  END IF;

END;  -- edit_recalc

/**
 * Recalculate from Jan 1, 2010.
 */
PROCEDURE recalculate_2010 (
  pCustomerId NUMBER
) IS
BEGIN

  recalculate(pcustomerid, '01-Jan-2010');

END recalculate_2010;

/**
 * Recalculate and send to item from Jan 1, 2010.
 */
PROCEDURE recalculateAndSendToItem_2010 (
  pCustomerId NUMBER
) IS
BEGIN

  recalculateAndSendToItem(pcustomerid, '01-Jan-2010');

END recalculateAndSendToItem_2010;

------------------------------ Balancing Utilities -----------------------------

/**
 * Adds warning when processing balance calculations.
 */
PROCEDURE addMonthlySummaryWarning (
  pReportingDate DATE,
  pCustomerId    NUMBER,
  pWarningCode   NUMBER,
  pRemark        VARCHAR2 := NULL
) IS
BEGIN

  INSERT INTO bs.trash_balance_warn (rep_date, customer_id, warn_id, remark)
  VALUES (pReportingDate, pCustomerId, pWarningCode, pRemark);

END addMonthlySummaryWarning;

/**
 * Returns normalized GEL charge.
 */
FUNCTION normalizeGel(pGel NUMBER, pOperationId NUMBER)
RETURN NUMBER
IS
BEGIN
  -- payment
  IF pOperationId IN (12, 16, 35, 49, 150, 20)
  THEN
    RETURN -pGel;
  -- subsidies
  ELSIF pOperationId IN (221, 222)
  THEN
    RETURN -ABS (pGel);
  -- other operations
  ELSE
    RETURN pGel;
  END IF;
END normalizeGel;

/**
 * Calculates item balance.
 */
FUNCTION calcItemBalance (pPrevBalance NUMBER, pGel NUMBER, pOperationId NUMBER)
RETURN NUMBER
IS
BEGIN

  RETURN NVL(pPrevBalance, 0) + normalizeGel(NVL(pGel, 0), pOperationId);

END calcItemBalance;

/**
 * Caluclate old balance correction.
 */
FUNCTION calcOldItemBalance (pCustomerId NUMBER, pItemId NUMBER, pPrevOldBalance NUMBER, pGel NUMBER, pOperationId NUMBER)
RETURN NUMBER
IS
  l_old_balanace NUMBER;
BEGIN
  IF pPrevOldBalance IS NULL
  THEN
    BEGIN
      SELECT old_balance INTO l_old_balanace
      FROM ( SELECT old_balance
        FROM bs.trashitem
        WHERE custkey = pCustomerId AND trashitemid > pItemId
        ORDER BY trashitemid ASC
      ) WHERE ROWNUM = 1;
    EXCEPTION WHEN NO_DATA_FOUND
    THEN
      SELECT old_balance INTO l_old_balanace
      FROM bs.trashcustomer
      WHERE custkey = pCustomerId;
    END;
    RETURN NVL(l_old_balanace, 0);
  ELSE
    -- old operation
    IF pOperationId IN (OPER_CHARGE_CORRECTION, OPER_SUBSIDY_CORRECTION, OPER_PREV_PERIOD_SUBSIDY, 19, 100)
    THEN
      l_old_balanace := NVL(pPrevOldBalance, 0) + normalizeGel(NVL(pGel, 0), pOperationId);
      IF l_old_balanace < 0
      THEN
        RETURN 0;
      ELSE
        RETURN l_old_balanace;
      END IF;
    -- this operation makes old balance equal to zero
    ELSIF pOperationId = OLD_BALANCE_GE_0_OPERATION
    THEN
      RETURN 0;
    -- leave as is
    ELSE
      RETURN NVL(pPrevOldBalance, 0);
    END IF;
  END IF;
END calcOldItemBalance;

/**
 * Retrives current state of the customer from the database.
 */
PROCEDURE getCustomerInfo (
  pReportingDate                DATE,
  pCustomerId             NUMBER,
  pCustomerInfo   OUT   TP_CUSTOMER_SUMMARY
) IS

  -- Units cursor
  CURSOR units_cursor (p_customer NUMBER, p_cut_date DATE)
  IS
    SELECT trash_cust_cat_id, pers_count, det_amount
    FROM (
      SELECT
        -- #1. Monthly summary category Id
        CASE
        WHEN --OLD: u.cat_id = cat_common
          u.id = CUM_RESIDENT_JAN2007
        THEN
          -- residential
          MONSUM_RESIDENTIAL_CATEGORY
        ELSE
          -- comercial
          MONSUM_COMERCIAL_CATEGORY
        END trash_cust_cat_id,
        -- #2. Person count
        CASE
        WHEN det.status = 0 -- only active persons count!
          AND u.unit_id = UNIT_COUNT_OF_PERSONS
        THEN
          -- persons
          det.amount
        ELSE
          -- no persons!
          0
        END pers_count,
        -- #3. detail amount, untract
        det.amount AS DET_AMOUNT
      FROM bs.trash_cust_det det, bs.trash_cat_units u
      WHERE det.cat_unit_id = u.ID
        AND customer_id = p_customer
        AND det.start_date <= p_cut_date
      --ORDER BY det.ID DESC
      ORDER BY det.status ASC, det.ID DESC)
    WHERE ROWNUM = 1;

  -- Previous customer summary
  CURSOR last_item_analyz (p_customer NUMBER)
  IS
    SELECT
      end_debet, end_credit, last_item_id,
      end_old_balance, end_curr_debet, end_curr_credit
    FROM (SELECT
        end_debet, end_credit, last_item_id,
        end_old_balance, end_curr_debet, end_curr_credit
      FROM bs.trash_cust_bef_balance
      WHERE customer_id = p_customer
      ORDER BY ID DESC)
    WHERE ROWNUM = 1;

  -- Last history record for customer with cut on some date
  CURSOR last_customer_item (p_customer NUMBER, p_cut_date DATE)
  IS
    SELECT item_id, amount_in_gel, oper_id, balance, old_balance
    FROM (SELECT trashitemid item_id, amount amount_in_gel,
        operationid oper_id, balance, old_balance
      FROM bs.trashitem
      WHERE custkey = p_customer AND operdate <= p_cut_date
      ORDER BY trashitemid DESC)
    WHERE ROWNUM = 1;

  p_pers_count          NUMBER;
  p_det_amount          NUMBER;
  p_trash_cust_cat_id   NUMBER;
  p_start_debet         NUMBER;
  p_start_credit        NUMBER;
  p_start_old_balance   NUMBER;
  p_start_curr_debet    NUMBER;
  p_start_curr_credit   NUMBER;
  p_end_debet           NUMBER;
  p_end_credit          NUMBER;
  p_end_old_balance     NUMBER;
  p_end_curr_debet      NUMBER;
  p_end_curr_credit     NUMBER;
  p_last_item_id        NUMBER;
  p_prev_last_item_id   NUMBER;
  p_amount_in_gel       NUMBER;
  p_oper_id             NUMBER;
  p_balance             NUMBER;
  p_old_balance         NUMBER;
  p_curr_balance        NUMBER;
  p_item_balance        NUMBER;
  p_subsidy_row         bs.trashsubsidies%ROWTYPE;
  p_success             BOOLEAN;
BEGIN

  OPEN units_cursor (pCustomerId, pReportingDate);

  FETCH units_cursor INTO p_trash_cust_cat_id, p_pers_count, p_det_amount;

  -- no trash customer detail found
  IF NOT units_cursor%FOUND
  THEN
    p_trash_cust_cat_id := MONSUM_NOTDEFINED_CATEGORY;
    p_pers_count := 0;
    p_start_debet := 0;
    p_start_credit := 0;

    -- WARNING: no customer detail found
    IF MONSUM_WARNINGS_ENABLED
    THEN
      addMonthlySummaryWarning(pReportingDate, pCustomerId, MONSUM_WARNING_NO_DETAIL);
    END IF;
    -- trash customer detail lookup
    --XXX: we remove the ELSE block, while it gives not desireable results:
    -- there were some customers which doesnot have any detail, but they
    -- had payments nevertheless
    --ELSE
  END IF;

  -- try to get last item from previous balance calculations
  OPEN last_item_analyz (pCustomerId);

  FETCH last_item_analyz INTO
    p_start_debet, p_start_credit, p_prev_last_item_id,
    p_start_old_balance, p_start_curr_debet, p_start_curr_credit;

  -- last item not found
  IF NOT last_item_analyz%FOUND
  THEN
    p_start_debet := 0;
    p_start_credit := 0;
    p_prev_last_item_id := -1;
    p_start_old_balance := 0;
    p_start_curr_debet := 0;
    p_start_curr_credit := 0;
  END IF;
  CLOSE last_item_analyz;
  CLOSE units_cursor;

  -- look up last item in customer's history
  OPEN last_customer_item (pCustomerId, pReportingDate);

  FETCH last_customer_item INTO p_last_item_id, p_amount_in_gel, p_oper_id, p_balance, p_old_balance;

  IF NOT last_customer_item%FOUND
  THEN
    p_end_debet       := 0;
    p_end_credit      := 0;
    p_end_old_balance := 0;
    p_end_curr_debet  := 0;
    p_end_curr_credit := 0;
    p_last_item_id    := -1;

    -- WARNING: no history record found
    IF MONSUM_WARNINGS_ENABLED
    THEN
      addMonthlySummaryWarning(pReportingDate, pCustomerId, MONSUM_WARNING_NO_PREV_HISTORY);
    END IF;

  ELSE
    p_item_balance    := calcItemBalance    (p_balance, p_amount_in_gel, p_oper_id);
    p_end_old_balance := calcOldItemBalance (pCustomerId, p_last_item_id, p_old_balance, p_amount_in_gel, p_oper_id);
    p_curr_balance    := p_item_balance - p_end_old_balance;

    IF p_item_balance > 0
    THEN
      p_end_debet  := ABS (p_item_balance);
      p_end_credit := 0;
    ELSE
      p_end_debet  := 0;
      p_end_credit := ABS (p_item_balance);
    END IF;
    
    IF p_curr_balance > 0
    THEN
      p_end_curr_debet  := ABS(p_curr_balance);
      p_end_curr_credit := 0;
    ELSE
      p_end_curr_debet  := 0;
      p_end_curr_credit := ABS(p_curr_balance);
    END IF;
  END IF;

  CLOSE last_customer_item;

  -- fill customer status structure
  pCustomerInfo.trash_cust_cat_id := p_trash_cust_cat_id;
  pCustomerInfo.pers_count := p_pers_count;
  pCustomerInfo.start_debet  := p_start_debet;
  pCustomerInfo.start_credit := p_start_credit;
  pCustomerInfo.start_old_balance := p_start_old_balance;
  pCustomerInfo.start_curr_credit := p_start_curr_credit;
  pCustomerInfo.start_curr_debet  := p_start_curr_debet;
  pCustomerInfo.end_debet  := p_end_debet;
  pCustomerInfo.end_credit := p_end_credit;
  pCustomerInfo.end_old_balance := p_end_old_balance;
  pCustomerInfo.end_curr_credit := p_end_curr_credit;
  pCustomerInfo.end_curr_debet  := p_end_curr_debet;
  pCustomerInfo.last_item_id := p_last_item_id;
  pCustomerInfo.prev_last_item_id := p_prev_last_item_id;
  pCustomerInfo.det_amount := p_det_amount;

  -- get subsidy information
  OPEN SUBSIDY_CURSOR(pCustomerId, pReportingDate, pReportingDate);
  FETCH SUBSIDY_CURSOR INTO p_subsidy_row;

  IF SUBSIDY_CURSOR%FOUND
  THEN
    pCustomerInfo.has_curr_subsidy := TRUE;
    pCustomerInfo.curr_subs_pers_count := p_subsidy_row.quantity;
  ELSE
    pCustomerInfo.has_curr_subsidy := FALSE;
    pCustomerInfo.curr_subs_pers_count := 0;
  END IF;

  CLOSE SUBSIDY_CURSOR;

END getCustomerInfo;

/**
 * Makes summary for monthly charge of trash billing. This summary is given with
 * details for each individual customer.
 */
PROCEDURE runTrashSummary (pReportingDate DATE)
IS
  pLastReportingDate  DATE;
  pCustomerInfo       TP_CUSTOMER_SUMMARY;
  p_subs_count        NUMBER;
  p_subs_pers_count   NUMBER;
  p_prev_item_id      NUMBER;
  p_sum_gel           NUMBER;
  p_prev_item_gel     NUMBER;
  p_flag              BOOLEAN;
  p_cat_unit_id       NUMBER;
  p_tariff_id         NUMBER;
  p_new_balance_id    NUMBER;
  p_enter_date        DATE := SYSDATE;
  pGel NUMBER;
  p_has_details NUMBER;
  p_has_charge_det NUMBER;
  p_has_zero_charge NUMBER;
  p_sum_charge_gel NUMBER;
BEGIN
  -- get last reporting date and check it
  SELECT MAX (rep_date) INTO pLastReportingDate FROM bs.trash_cust_bef_balance;

  -- check dates
  IF pLastReportingDate IS NOT NULL AND pLastReportingDate >= pReportingDate
  THEN
    raise_application_error (-20000, 'Last monthly summary reporting date '
      || pLastReportingDate || ' is greater than the current reporting date '
      || pReportingDate);
  END IF;

  -- loop over all customers
  FOR rec IN (SELECT
      tc.custkey customer_id, tc.status status, cust.custcatkey cust_cat_id,
      tc.balance curr_balance, address.regionkey serv_cent_id
    FROM bs.trashcustomer tc, bs.customer cust, bs.address
    WHERE tc.custkey = cust.custkey
      AND cust.premisekey = address.premisekey
    ORDER BY tc.custkey
  ) LOOP
    -- get customer information for reporting date
    getCustomerInfo(pReportingDate, rec.customer_id, pCustomerInfo);

    -- validate current balance with last item
    IF pCustomerInfo.last_item_id IS NOT NULL
      AND MONSUM_WARNINGS_ENABLED
      AND ABS (ABS (pCustomerInfo.end_debet) - ABS (pCustomerInfo.end_credit) - rec.curr_balance) >= MIN_GEL
    THEN
      -- WARNING: customer current balance problems
      addMonthlySummaryWarning (pReportingDate, rec.customer_id, MONSUM_WARNING_CURRBAL_PROBLEM);
    ELSIF pCustomerInfo.last_item_id IS NULL
      AND MONSUM_WARNINGS_ENABLED
      AND ABS (rec.curr_balance) > MIN_GEL
    THEN
      -- WARNING: customer current balance problems
      addMonthlySummaryWarning (pReportingDate, rec.customer_id, MONSUM_WARNING_CURRBAL_PROBLEM);
    END IF;

    -- retrive subsidy information
    IF pCustomerInfo.has_curr_subsidy
    THEN
      p_subs_count := 1;
    ELSE
      p_subs_count := 0;
    END IF;

    -- insert values
    INSERT INTO bs.trash_cust_bef_balance (
      customer_id, rep_date, serv_cent_id, cust_cat_id, trash_cust_cat_id,
      pers_count, status,
      start_debet, end_debet, start_credit, end_credit,
      last_item_id, prev_last_item_id,
      curr_subs_count, curr_subs_pers_count,
      enter_date, has_details, has_charge_det,
      start_old_balance, start_curr_debet, start_curr_credit,
      end_old_balance, end_curr_debet, end_curr_credit
    ) VALUES (
      rec.customer_id, pReportingDate, rec.serv_cent_id, rec.cust_cat_id, pCustomerInfo.trash_cust_cat_id,
      pCustomerInfo.pers_count, NVL (rec.status, 0),
      NVL (pCustomerInfo.start_debet, 0), NVL (pCustomerInfo.end_debet, 0), NVL (pCustomerInfo.start_credit, 0), NVL (pCustomerInfo.end_credit, 0),
      NVL (pCustomerInfo.last_item_id, -1), NVL (pCustomerInfo.prev_last_item_id, -1),
      p_subs_count, pCustomerInfo.curr_subs_pers_count,
      p_enter_date, 0, 0,
      pCustomerInfo.start_old_balance, pCustomerInfo.start_curr_debet, pCustomerInfo.start_curr_credit,
      pCustomerInfo.end_old_balance, pCustomerInfo.end_curr_debet, pCustomerInfo.end_curr_credit
    ) RETURNING ID INTO p_new_balance_id;

    p_has_details := 0;
    p_has_charge_det := 0; 
    p_sum_charge_gel := 0;

    -- create details
    IF pCustomerInfo.last_item_id != -1
    THEN
      p_prev_item_id := -1;
      p_sum_gel := 0;
      --p_sum_m3 := 0;
      p_flag := FALSE;

      FOR item IN (SELECT it.trashitemid item_id, it.operationid oper_id,
          it.amount item_gel, it.trashtariffvalueid item_m3, it.operdate oper_date,
          det.ID det_id, det.cust_det_id cust_det_id, det.cat_coeff_id cat_coeff_id,
          det.amnt_in_m3 det_m3, det.amnt_in_gel det_gel, det.subs_det_id,
          det.subs_count, det.start_date, det.end_date
        FROM bs.trashitem it, bs.trash_item_det det
        WHERE custkey = rec.customer_id
          AND it.trashitemid = det.item_id(+)
          AND (it.trashitemid > NVL(pCustomerInfo.prev_last_item_id, -1)
          AND it.trashitemid <= NVL (pCustomerInfo.last_item_id, -1))
        ORDER BY it.trashitemid)
      LOOP
        -- item changed!
        IF p_prev_item_id != item.item_id
        THEN
          -- check only when flaged
          IF p_flag
          THEN
            -- check wether item GEL and details summary GEL are the same
            IF ABS(p_sum_gel - p_prev_item_gel) >= min_gel AND MONSUM_WARNINGS_ENABLED
            THEN
              -- WARNING: gel not matching
              addMonthlySummaryWarning (pReportingDate, rec.customer_id, MONSUM_WARNING_GEL_NOT_MATCH);
            END IF;
          END IF;

          p_sum_gel := 0;
          --p_sum_m3 := 0;
          p_prev_item_gel := NVL (item.item_gel, 0);
          --p_prev_item_m3 := NVL (item.item_m3, 0);
          p_prev_item_id := item.item_id;
        END IF;

        -- when a single item without details
        IF item.det_id IS NULL
        THEN
          -- no details!
          p_flag := FALSE;
          pGel := normalizeGel(NVL(item.item_gel, 0), item.oper_id);
          INSERT INTO bs.trash_cust_bef_balance_det (
            customer_id, oper_id, cat_unit_id, cat_coeff_id, tariff_id, amnt_in_m3,
            amnt_in_gel, subs_count, subs_pers_count, item_id,
            balance_id, start_date, end_date
          ) VALUES (
            rec.customer_id, item.oper_id, -1, -1, -1, 0,
            pGel, 0, 0, item.item_id,
            p_new_balance_id, TRUNC (item.oper_date), NULL
          );
          
          -- no details!
          p_has_details := 0;
          p_has_charge_det := 0;
          
        -- item with one or more details
        ELSE
          -- details accessed!
          p_flag := TRUE;
          -- add summaries
          p_sum_gel := p_sum_gel + NVL (item.det_gel, 0);
          -- get cat_unit_id and tariff_id
          p_cat_unit_id := -1;
          p_tariff_id := -1;

          IF item.cust_det_id IS NOT NULL
          THEN
            SELECT cat_unit_id, tariff_id INTO p_cat_unit_id, p_tariff_id
            FROM bs.trash_cust_det
            WHERE ID = item.cust_det_id;
          END IF;

          IF item.subs_det_id IS NULL
          THEN
            p_subs_count := 0;
            p_subs_pers_count := 0;
          ELSE
            p_subs_count := 1;
            p_subs_pers_count := NVL (item.subs_count, 0);
          END IF;

          -- process insertion
          pGel := normalizeGel(NVL(item.det_gel, 0), item.oper_id);
          INSERT INTO bs.trash_cust_bef_balance_det (
            customer_id, oper_id, cat_unit_id, cat_coeff_id, tariff_id,
            amnt_in_m3, amnt_in_gel, subs_count, subs_pers_count, item_id,
            balance_id, start_date, end_date)
          VALUES (
            rec.customer_id, item.oper_id, p_cat_unit_id, NVL (item.cat_coeff_id, -1), p_tariff_id,
            0, pGel, p_subs_count, p_subs_pers_count, item.item_id,
            p_new_balance_id, NVL (item.start_date, item.oper_date), item.end_date);

          IF item.oper_id = 212
          THEN
            p_sum_charge_gel := p_sum_charge_gel + pGel;
          END IF;

          p_has_details := 1;
          IF item.oper_id = OPER_CHARGE
          THEN
            p_has_charge_det := 1;
          END IF;
        END IF;
      END LOOP;

      -- @since 28/06/2010: update customer detail records
      -- @since 05/07/2010: added support for "has_zero_charge" column
      
      -- we are intereseted in ZERO DETAIL, so the name "zero_charge" is not exactly accurate
      IF ABS(pCustomerInfo.DET_AMOUNT) < 0.0099
      THEN
        p_has_zero_charge := 1;
      ELSE
        p_has_zero_charge := 0;
      END IF;

      UPDATE bs.trash_cust_bef_balance
      SET
        has_details = p_has_details,
        has_charge_det = p_has_charge_det,
        has_zero_charge = p_has_zero_charge
      WHERE ID = p_new_balance_id;

      -- @since: END

      -- check only when flaged
      IF p_flag
      THEN
        -- check wether item GEL and details summary GEL are the same
        IF ABS(p_sum_gel - p_prev_item_gel) >= min_gel AND MONSUM_WARNINGS_ENABLED
        THEN
          -- WARNING: gel not matchings
          addMonthlySummaryWarning (pReportingDate, rec.customer_id, MONSUM_WARNING_GEL_NOT_MATCH);
        END IF;
      END IF;

    END IF;

    -- @since 05/07/2010
    IF pCustomerInfo.trash_cust_cat_id = MONSUM_NOTDEFINED_CATEGORY
    THEN
      UPDATE bs.trash_cust_bef_balance
      SET
        trash_cust_cat_id = MONSUM_RESIDENTIAL_CATEGORY,
        status = 2
      WHERE ID = p_new_balance_id;
    END IF;
    
    IF ABS(p_sum_charge_gel) < MIN_GEL AND pCustomerInfo.pers_count > 0
    THEN
      UPDATE bs.trash_cust_bef_balance
      SET
        PERS_COUNT = 0
      WHERE ID = p_new_balance_id;
    END IF;
    
    -- @since: END

  END LOOP;

END runTrashSummary;

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

PROCEDURE assert_equals (
  pExpected NUMBER,
  pActual NUMBER
) IS
BEGIN

  IF NVL(pExpected, 0) != NVL(pActual, 0)
  THEN
    RAISE_APPLICATION_ERROR(-20000, 'assertion failed: excepted ' || pExpected || ' but was ' || pActual);
  END IF;

END assert_equals;

PROCEDURE assert_equals (
  pExpected DATE,
  pActual DATE
) IS
BEGIN

  IF NVL(pExpected, '1-Jan-1980') != NVL(pActual, '1-Jan-1980')
  THEN
    RAISE_APPLICATION_ERROR(-20000, 'assertion failed: excepted ' ||
      to_char(pExpected) || ' but was ' || to_char(pActual));
  END IF;

END assert_equals;

PROCEDURE assert_true (
  pCondition BOOLEAN
) IS
BEGIN
  IF NOT pcondition
  THEN
    RAISE_APPLICATION_ERROR(-20000, 'condition failed: false');
  END IF;
END assert_true;

PROCEDURE assert_false (
  pCondition BOOLEAN
) IS
BEGIN
  IF pcondition
  THEN
    RAISE_APPLICATION_ERROR(-20000, 'condition failed: true');
  END IF;
END assert_false;

/**
 * Test date appending into date array.
 */
PROCEDURE test_appendDateSortedAndUnique
IS
  pDate1 DATE := '1-Jan-2009';
  pDate2 DATE := '31-Jan-2009';
  pDate3 DATE := '1-Jan-2009';
  pDate4 DATE := '15-Jan-2009';
  pDates TP_DATE_ARRAY;
BEGIN

  appendDateSortedAndUnique(pDate1, pDates);
  appendDateSortedAndUnique(pDate2, pDates);
  appendDateSortedAndUnique(pDate3, pDates);
  appendDateSortedAndUnique(pDate4, pDates);

  assert_equals(3, pDates.COUNT);
  assert_equals('1-Jan-2009', pDates(1));
  assert_equals('15-Jan-2009', pDates(2));
  assert_equals('31-Jan-2009', pDates(3));

END test_appendDateSortedAndUnique;

/**
 * Testing find_intersection procedure.
 */
PROCEDURE test_findIntersection
IS
  pIntersection BOOLEAN;
  pD1 DATE;
  pD2 DATE;
BEGIN

  -- full interval
  findIntersection('1-Jan-2009', '31-Jan-2009', '1-Jan-2009', '31-Jan-2009', pIntersection, pD1, pD2);
  assert_true(pIntersection);
  assert_equals('1-Jan-2009', pD1);
  assert_equals('31-Jan-2009', pD2);

  -- inner interval
  findIntersection('1-Jan-2009', '31-Jan-2009', '15-Jan-2009', '20-Jan-2009', pIntersection, pD1, pD2);
  assert_true(pIntersection);
  assert_equals('15-Jan-2009', pD1);
  assert_equals('20-Jan-2009', pD2);

  -- intersecting intervals
  findIntersection('1-Jan-2009', '31-Jan-2009', '15-Jan-2009', '15-Feb-2009', pIntersection, pD1, pD2);
  assert_true(pIntersection);
  assert_equals('15-Jan-2009', pD1);
  assert_equals('31-Jan-2009', pD2);

  -- non-intersecting intervals
  findIntersection('1-Jan-2009', '31-Jan-2009', '15-Feb-2009', '15-Mar-2009', pIntersection, pD1, pD2);
  assert_false(pIntersection);

END;

/**
 * All tests.
 */
PROCEDURE test_all
IS
BEGIN

  test_appendDateSortedAndUnique;
  test_findIntersection;

END test_all;

END trash_manager_2007_2;
/

