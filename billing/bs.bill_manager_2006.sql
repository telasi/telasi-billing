create or replace PACKAGE BILL_MANAGER_2006 IS

/**
 * <p>
 * This package mainly substitutes BS.BILL_MANAGER package after the
 * 01-June-2006, when the new structure of the billing procedures was proposed.
 * It also combines capabilities to work both with flat and step tariff
 * structures and also has new possibilities on additional and estimate (derived
 * from installed capacity) charges normalization.
 * </p>
 *
 * <p>
 * This package may be used for both flat and step tariff calculations. It
 * supports two types of step tariff (continuous or 'camel' and throughout or
 * 'step').
 * </p>
 *
 * <p>
 * See detailed instructions in package documentation.
 * </p>
 *
 * <p>
 * Changes in this package can be seen in the table below:
 * </p>
 *
 * @table Please, describe in the table below any change in this package
 * +-------------+-----------------+--------------------------------------------+
 * | Date        | Person          | Short Description                          |
 * +-------------+-----------------+--------------------------------------------+
 * | 15 Feb 2006 | D. Kurashvili   | Start of the package development           |
 * +-------------+-----------------+--------------------------------------------+
 * | 15 Mar 2006 | D.Kurashvili    | Start of the testing this package          |
 * |             |                 |  I.Japaridze, M.Azniashvili, Rusiko        |
 * +-------------+-----------------+--------------------------------------------+
 * | 01 Jun 2006 | D.Kurashvili    | First Release                              |
 * +-------------+-----------------+--------------------------------------------+
 * | 20 Jun 2006 | D.Kurashvili
 * |
 * | Change in 'close_account' procedure (old code commented):
 * |
 * | ELSIF --ABS(NVL(rec.AMOUNT, 0)) > MIN_GEL
 * |    NVL(rec.AMOUNT, 0) > MIN_GEL -- only positive GEL charges!!!
 * |
 * | This is a temporary change for a (June) month. It is needed while we have
 * | some negative charges in the old period. This code ignores all negative GEL
 * | charges; may be this is not the best solution. After 1 Jul this code will
 * | be restored to the initial state.
 * |
 * | 4 Jul 2006, old version was restored
 * +-------------+-----------------+--------------------------------------------+
 * | 7-11 Jule,  | Z. Khvedelidze  | Improved performance using cut on          |
 * | 2006        |                 | ENTERDATE in BS.ITEM table                 |
 * +-------------+-----------------+--------------------------------------------+
 * | 20-Jul-2006 | D. Kurashvili   | (Irina Japaridze):
 * |             |                 | when the previous cycle is too far from the current one
 * |             |                 | (60 days) then we should consider this situation in more
 * |             |                 | details.
 * |             |                 |
 * |             |                 | So, when the previous cycle is too far. We should redefine
 * |             |                 | the start point of the cycle calculations. (End point is current cycle
 * |             |                 | date). Firts of all we are trying to find first not cycle
 * |             |                 | reading during this cycle.
 * |             |                 | 
 * |             |                 | 1. if this not-cycle reading exists, then we make it as a start point
 * |             |                 | 2. if this not-cycle reading doesnot exist, then we try to consider
 * |             |                 | account creation date.
 * |             |                 |   2.1. if an account creation date is not very far from
 * |             |                 |   the current cycle, then we take it as a start point
 * |             |                 |   2.2. otherwise we take the date 60 days back from the current
 * |             |                 |   cycle date as a start point. All operations before this date
 * |             |                 |   will be neglected.
 * +-------------+-----------------+--------------------------------------------+
 * | 28-Jul-2006 | D.Kurashvili    | see @since 28/07/2006: 'Reading' derivation|
 * |             |                 | was incorrect for some cycle reading cases.|
 * +-------------+-----------------+--------------------------------------------+
 * | 11-Aug-2006 | D.Kurashvili    | Changes that we had made on '20-Jul-2006'
 * |             |                 | have no effect on cycles recalculation when
 * |             |                 | correct estimaes. We add support for this today.
 * +-------------+-----------------+--------------------------------------------+
 * | 31-Aug-2006 | D.Kurashvili    | Check on empty GEL amounts in current      |
 * |             |                 | charges (voucher, act)                     |
 * +-------------+-----------------+--------------------------------------------+
 * | 21-Feb-2007 | D.Kurashvili    | We had some interferension of step-tariff
 * |             |                 | with zero-tariff.
 * |             |                 |
 * |             |                 | We add some new conditions to detect whether
 * |             |                 | zero GEL charge is really produced by
 * |             |                 | step-tariff.
 * |             |                 |
 * +-------------+-----------------+--------------------------------------------+
 * | 10/04/2007  | D.Kurashvili    | After block calculation we update account  |
 * |             |                 |   meter condition according to ROUTE_STORE |
 * +-------------+-----------------+--------------------------------------------+
 *
 */

  ------------------------------------------------------------------------------
  --                         Type declarations                                --
  ------------------------------------------------------------------------------

  -- Type used for holding record from BS.ITEM table.
  TYPE tp_items IS TABLE OF ITEM%ROWTYPE;

  -- Type for holding record from the BS.TARIFF_STEP table.
  TYPE tp_tar_steps IS TABLE OF TARIFF_STEP%ROWTYPE;

  -- Type for storing charge parameters. These are operation code, corresponding
  -- to BILLOPERKEY fields in BS.BILLOPERATION table and amount of the charge.
  -- This amount may be expressed in GEL, kWh or percent units; this record does
  -- not define type of the charge unit.
  TYPE tp_charge IS RECORD (oper_key NUMBER, amount NUMBER);

  -- Collection type for charges.
  TYPE tp_charges IS TABLE OF tp_charge;

  -- Parent account description record. This are account and customer numbers
  -- for the parent account.
  TYPE tp_parent IS RECORD(account NUMBER, customer NUMBER);

  -- Type for holding tariff steps structure.
  TYPE tp_steps_cache IS TABLE OF tp_tar_steps;

  -- Item details record
  TYPE tp_item_detail IS RECORD(tariff_key NUMBER, tariff_step_key NUMBER,
    gel NUMBER, vat NUMBER, kwh NUMBER, acctarkey NUMBER);

  -- Collection of Item details records
  TYPE tp_item_details IS TABLE OF tp_item_detail;

  -- Correction type. It contains information about correction year and
  -- discharge/recharge parameters. These parameters are divided into three
  -- categories: plain kWh charge, % subsidy and additional charge parameters.
  TYPE tp_correction IS RECORD (
    year NUMBER,
    discharge_kwh NUMBER, psubs_discharge_kwh NUMBER, add_discharge_kwh NUMBER,
    recharge_kwh  NUMBER, psubs_recharge_kwh  NUMBER, add_recharge_kwh  NUMBER,
    discharge_gel NUMBER, psubs_discharge_gel NUMBER, add_discharge_gel NUMBER,
    recharge_gel  NUMBER, psubs_recharge_gel  NUMBER, add_recharge_gel  NUMBER,
    recharge_det  tp_item_details
  );

  -- Collection type for holding correction records.
  TYPE tp_corrections IS TABLE OF tp_correction;

  ------------------------------------------------------------------------------
  --                                Options                                   --
  ------------------------------------------------------------------------------

  -- Tariff cache.
  TARIFF_CACHE tp_steps_cache;

  -- This flag defines whether process discharge/recharge operation on parent
  -- when correcting estimate charges on child and kWh absolute values for both
  -- discharge and recharge on child are equal.
  CORRECT_EQ_PRNT_CHARGES CONSTANT BOOLEAN := FALSE;

  -- Date which indicated start of the period which can be recalculated using
  -- this package.
  CONTROLABLE_FROM        CONSTANT DATE    := '01-Sep-2005';

  -- Days count in 'month'. This is used for normalization of period-sensitive
  -- charges.
  NORMALIZATION_PERIOD    CONSTANT NUMBER  := 30;

  -- Maximal distinction allowed for the previous cycle in days
  -- @since 20/07/2006: initiate from Irina (Japaridze)
  PREVIOUS_CYCLE_MAX_DISTINCTION CONSTANT NUMBER := 60;

  -- Flag which indicates, that normalization of additional charges should be
  -- processed on the previous month days count. When this flag is FALSE, then
  -- normalization is processed using standard NORMALIZATION_PERIOD constant.
  -- In old version of the bill manager, there was only one state of this flag:
  -- TRUE value (i.e. normalize on previous month days count).
  NORMALIZE_ON_PREV_MONTH CONSTANT BOOLEAN := FALSE;

  -- Flag which indicates that additional charges should be also normalized,
  -- like installed capacity charge. Normalizing on days becomes more actual
  -- when step structured tariff is used.
  -- In old version of the bill manager, there was only one state of this flag:
  -- FALSE value (i.e. normalization was completely ignored).
  NORMALIZE_ADD_CHARGES   CONSTANT BOOLEAN := TRUE;

  ------------------------------------------------------------------------------
  --                               Constants                                  --
  ------------------------------------------------------------------------------

  -- Precission for GEL and for kWh charges

  -- This is a precission for defining whether kWh charge exists and
  -- for comparizon of two kWh charges
  MIN_KWH CONSTANT NUMBER := 0.0099;

  -- This parameter plays the same role for GEL charges as MIN_KWH plays for
  -- kWh charges. It is recomended to derive this parameter from MIN_KWH
  -- constant by multiplying it by the "average" tariff used in your system.
  MIN_GEL CONSTANT NUMBER := MIN_KWH * 0.1;
  
  -- Minimum value for the debt.
  MIN_DEBT CONSTANT NUMBER := 1.00;

  -- Correction categories constants

  -- Account discharge category.
  DISCHARGE_PLAIN  CONSTANT NUMBER  := 1;

  -- Percent subsidy discharge category.
  DISCHARGE_PSUBS  CONSTANT NUMBER  := 2;

  -- Parent account discharge category.
  DISCHARGE_PARENT CONSTANT NUMBER  := 3;

  -- Account recharge cateogry.
  RECHARGE_PLAIN   CONSTANT NUMBER  := 4;

  -- Percent subsidy recharge category.
  RECHARGE_PSUBS   CONSTANT NUMBER  := 5;

  -- Parent account recharge category.
  RECHARGE_PARENT  CONSTANT NUMBER  := 6;

  -- Population options for BS.ITEM table

  -- Common option, which means unrechargable, unsummarizable records. Record of
  -- this type is not inserted into BS.ITEM_RELATION table and plays no role
  -- for cycle summary and recharge operations. This are mainly GEL charges or
  -- subsidies without kWh charging.
  ITEM_REC_COMMON                CONSTANT NUMBER := 0;

  -- Common valuable charge. This are charges like 'reading', 'estimate' etc.
  -- This charge is used when summarize cycle state and performing discharge/
  -- recharge operations. This charge may be corrected. Information about
  -- corrected charge of this type is strored in BS.ITEM_RELATION table.
  ITEM_REC_VALUABLE              CONSTANT NUMBER := 1;

  -- Same as ITEM_REC_VALUABLE category, but it additionaly indicates percent
  -- nature of this charge.
  ITEM_REC_PERCENT_CHARGE        CONSTANT NUMBER := 2;

  -- Record for the parent account. Cateogry of this type comes after
  -- ITEM_REC_VALUABLE or ITEM_REC_PERCENT_CHARGE cateogries on child account.
  ITEM_REC_PARENT_CHARGE         CONSTANT NUMBER := 3;

  -- Record category which follows ITEM_REC_PARENT_CHARGE category, when parent
  -- has subsidy when it is (dis)charged by child account.
  ITEM_REC_PARENT_PERCENT_CHARGE CONSTANT NUMBER := 4;

  -- Dates and periods

  -- Date which is used by the system to identify undefined date in the past.
  INFINITY_PAST    CONSTANT DATE := '01-Jan-1980';

  -- Date which is used by the system to identify undefined date in the future.
  INFINITY_FUTURE  CONSTANT DATE := '01-Jan-2999';

  -- Date used by the system to approve performance in the BS.ITEM table.
  ITEM_CUT_DATE    CONSTANT DATE := TRUNC(SYSDATE) - 90;

  -- Metter types

  -- Constant, which indicates that the account has no metter.
  MT_TYPE_NONE CONSTANT NUMBER := 15;

  -- Measurement units

  -- Constant which indicates GEL measurement unit.
  MSR_UNIT_GEL     CONSTANT NUMBER := 1;

  -- Constant which indicates kWh measurement unit.
  MSR_UNIT_KWH     CONSTANT NUMBER := 2;

  -- Constant which indicates percent(%) measurement unit.
  MSR_UNIT_PERCENT CONSTANT NUMBER := 3;

  -- @since 1-Jul-2006
  MSR_UNIT_DAY     CONSTANT NUMBER := 4;

  -- @since 1-Jul-2006
  MSR_UNIT_DIFF    CONSTANT NUMBER := 5;

  -- Route calculation error codes. This codes are used when analizing cycle
  -- reading information. User is notified by existence of the problem by
  -- marking record with one of these codes. Meaning of these codes can be
  -- foung in documentation.
  ERR_01 CONSTANT NUMBER := 1;
  ERR_02 CONSTANT NUMBER := 2;
  ERR_03 CONSTANT NUMBER := 3;
  ERR_04 CONSTANT NUMBER := 4;
  ERR_05 CONSTANT NUMBER := 5;
  ERR_06 CONSTANT NUMBER := 6;
  ERR_07 CONSTANT NUMBER := 7;
  ERR_08 CONSTANT NUMBER := 8;
  ERR_09 CONSTANT NUMBER := 9;
  ERR_10 CONSTANT NUMBER := 10;
  ERR_11 CONSTANT NUMBER := 11;
  ERR_12 CONSTANT NUMBER := 12;
  ERR_13 CONSTANT NUMBER := 13;
  ERR_14 CONSTANT NUMBER := 14;
  ERR_15 CONSTANT NUMBER := 15;
  ERR_16 CONSTANT NUMBER := 16;
  ERR_17 CONSTANT NUMBER := 17; -- NEW! estimates period is too large
  ERR_18 CONSTANT NUMBER := 18; -- NEW! @see documentation
  ERR_19 CONSTANT NUMBER := 19; -- NEW! @see documentation
  ERR_20 CONSTANT NUMBER := 20; -- NEW! @see documentation
  ERR_21 CONSTANT NUMBER := 21; -- NEW! @see documentation
  ERR_22 CONSTANT NUMBER := 22; -- NEW! @see documentation
  ERR_23 CONSTANT NUMBER := 23; -- NEW! @see documentation

  -- Customer categories

  -- Constant which indicated TP customer category.
  CUST_CAT_TP          CONSTANT NUMBER := 4;

  -- Constant which indicated feeder customer category.
  CUST_CAT_FEEDER      CONSTANT NUMBER := 5;

  -- Constant which indicated electricity substation customer category.
  CUST_CAT_SUB_STATION CONSTANT NUMBER := 6;

  -- Operation categories codes

  -- Reading category constant.
  OPER_CAT_READING    CONSTANT NUMBER := 1;

  -- Charge category constant (this may be 'estimate', 'without reading' or
  -- 'not operable metter' operations).
  OPER_CAT_CHARGE     CONSTANT NUMBER := 2;

  -- Payment category constant.
  OPER_CAT_PAYMENT    CONSTANT NUMBER := 3;

  -- Subsidy category constant.
  OPER_CAT_SUBSIDY    CONSTANT NUMBER := 5;

  -- Additional charge category constant.
  OPER_CAT_ADD_CHARGE CONSTANT NUMBER := 6;

  -- Audit reading category constant.
  OPER_CAT_AUDIT      CONSTANT NUMBER := 15;

  -- Operation codes

  -- Reading.
  OPER_READING                   CONSTANT NUMBER := 1;

  -- Control reading.
  OPER_CONTROL_READING           CONSTANT NUMBER := 2;

  -- Sale operation.
  OPER_SALE                      CONSTANT NUMBER := 3;

  -- Metter install reading.
  OPER_METER_INSTALL             CONSTANT NUMBER := 4;

  -- Metter deinstall reading.
  OPER_METER_DEINSTALL           CONSTANT NUMBER := 5;

  -- Customer cut reading.
  OPER_CUT                       CONSTANT NUMBER := 6;

  -- Customer repair reading.
  OPER_REPAIR                    CONSTANT NUMBER := 7;

  -- Without meter charge.
  OPER_WITHOUT_METER             CONSTANT NUMBER := 9;

  -- Not operable meter charge.
  OPER_NOTOPERABLE_METER         CONSTANT NUMBER := 10;

  -- Estimate charge.
  OPER_ESTIMATE                  CONSTANT NUMBER := 11;

  -- Parent account charge.
  OPER_PARENT_CHARGE             CONSTANT NUMBER := 38;

  -- Debt reschedule operations
  OPER_DEBT_RESCHEDULE           CONSTANT NUMBER := 47;
  OPER_DEBT_RESCHEDULE_PERCENT   CONSTANT NUMBER := 48;

  -- Balance reading.
  OPER_BALANCE                   CONSTANT NUMBER := 50;

  -- Audit reading.
  OPER_AUDIT                     CONSTANT NUMBER := 124;

  -- Debt deactivation operation code
  OPER_DEBT_DEACTIVATION         CONSTANT NUMBER := 217;

  -- Debt activation operation code
  OPER_DEBT_ACTIVATION           CONSTANT NUMBER := 218;

  -- Charge for the current cycle.
  OPER_CURR_CYCLE_CHARGE         CONSTANT NUMBER := 504;

  -- Charge for the current cycle from act procession.
  OPER_CURR_CYCLE_CHARGE_ACT     CONSTANT NUMBER := 505;

   -- Additional Charge for the current cycle.
  OPER_CURR_CHARGE_ADD          CONSTANT NUMBER := 67;

  -- Charge for the current cycle from voucher procession.
  OPER_CURR_CYCLE_CHARGE_VOUCHER CONSTANT NUMBER := 506;

  -- Summary record for the current cycle.
  OPER_SUMMARY                   CONSTANT NUMBER := 507;

  -- Dicharge of all valuable GEL charges for the current cycle, except
  -- subsidies (@see OPER_SUBSIDY_GEL_DISCHARGE)
  OPER_CURR_CYCLE_GEL_DISCHARGE  CONSTANT NUMBER := 508;

  -- Summary record for the percent subsidy for the current cycle.
  OPER_PSUBS_SUMMARY             CONSTANT NUMBER := 513;

  -- Discharge all valueble subsidy GEL charges
  OPER_SUBSIDY_GEL_DISCHARGE     CONSTANT NUMBER := 514;

  -- Cycle constants

  -- Yet not closed cycle constant
  CYCLE_NONE CONSTANT NUMBER := -100;

  ------------------------------------------------------------------------------
  --                           Method Declarations                            --
  ------------------------------------------------------------------------------

  /* TEMPORARY PROCEDURE */
  PROCEDURE close_cycle_temp (p_route_store_key NUMBER);

  /**
   * See details on the methods in the 'body' section.
   */
   
  PROCEDURE recalculate_last_cycle (
    p_account      NUMBER,
    p_new_reading  NUMBER,
    p_oper_key     NUMBER,
    p_force_recalc BOOLEAN := FALSE
  );

  PROCEDURE delete_item_and_related (
    p_item NUMBER
   );

  PROCEDURE delete_item_and_related (
    p_item            NUMBER,
    p_delete_percents BOOLEAN,
    p_delete_parent   BOOLEAN
  );
  
  PROCEDURE update_balances_history (
    p_customer   NUMBER,
    p_start_item NUMBER
  );

  PROCEDURE calc_route (
    p_sched_key    IN    NUMBER,
    p_close_route  IN    BOOLEAN,
    p_insp_key     IN    NUMBER,
    p_oper_key     IN    NUMBER,
    p_oper_job     IN    NUMBER,
    is_power_user  IN    BOOLEAN
  );

  PROCEDURE calc_block (
    p_block_key  IN    NUMBER,
    p_cycle_date IN    DATE,
    p_oper_key   IN    NUMBER,
    p_insp_key   IN    NUMBER
  );

  PROCEDURE process_debt_agreement (
    rs_record IN     ROUTE_STORE%ROWTYPE
  );

  PROCEDURE new_not_cycle_reading (
    p_acc_key     IN    NUMBER,
    p_operation   IN    NUMBER,
    p_read_date   IN    DATE,
    p_read_val    IN    NUMBER,
    p_init_kwh    IN    NUMBER,
    p_init_gel    IN    NUMBER,
    p_operator    IN    NUMBER,
    p_sign_person IN    NUMBER
  );

  PROCEDURE new_cycle_reading (
    p_route_store_rec   IN    route_store%ROWTYPE,
    p_for_send_to_print IN    BOOLEAN,
    p_oper_key          IN    NUMBER,
    p_insp_key          IN    NUMBER
  );

  PROCEDURE close_account (
    p_customer          IN    NUMBER,
    p_account           IN    NUMBER,
    p_is_main_acc       IN    BOOLEAN,
    p_acc_creation      IN    DATE,
    p_mt_digits         IN    NUMBER,
    p_mt_coeff          IN    NUMBER,
    p_new_kwh_init      IN    NUMBER,
    p_new_read_val      IN    NUMBER,
    p_new_read_type     IN    NUMBER,
    p_schedule          IN    NUMBER,
    p_new_read_date     IN    DATE,
    p_prev_read_date    IN    DATE,
    p_prev_real_read    IN    ITEM%ROWTYPE,
    p_prev_read         IN    ITEM%ROWTYPE,
    p_force_est_correct IN    BOOLEAN,
    p_operator          IN    NUMBER,
    p_inspector         IN    NUMBER
  );

  PROCEDURE calc_kwh (
    p_account              IN     NUMBER,
    p_is_cycle_reading     IN     BOOLEAN,
    p_cust_cat_key         IN     NUMBER,
    p_acc_inst_cp          IN     NUMBER,
    p_acc_cr_date          IN     DATE,
    p_is_cutted            IN     BOOLEAN,
    p_min_kwh              IN     NUMBER,
    p_max_kwh              IN     NUMBER,
    p_mt_type              IN     NUMBER,
    p_mt_digits            IN     NUMBER,
    p_mt_coeff             IN     NUMBER,
    p_mt_stat              IN     BOOLEAN,
    p_seal_stat            IN     BOOLEAN,
    p_curr_read_val        IN     NUMBER,
    p_curr_read_type       IN     NUMBER,
    p_curr_read_date       IN     DATE,
    p_curr_kwh             IN     NUMBER,
    p_curr_amnt            IN     NUMBER,
    p_prev_read            IN     ITEM%ROWTYPE,
    p_prev_act_read        IN     ITEM%ROWTYPE,
    p_new_kwh              IN OUT NUMBER,
    p_new_read_val         IN OUT NUMBER,
    p_new_read_type        IN OUT NUMBER,
    p_new_err_code         IN OUT NUMBER,
    p_force_zero_amnt      IN OUT BOOLEAN,
    p_est_reading          IN OUT NUMBER,
    p_force_est_correction IN OUT BOOLEAN
  );

  PROCEDURE calc_cycle_reading (
    p_account              IN       NUMBER,
    p_cust_cat_key         IN      NUMBER,
    p_acc_inst_cp          IN      NUMBER,
    p_acc_cr_date          IN      DATE,
    p_is_cutted            IN      BOOLEAN,
    p_min_kwh              IN      NUMBER,
    p_max_kwh              IN      NUMBER,
    p_mt_type              IN      NUMBER,
    p_mt_digits            IN      NUMBER,
    p_mt_coeff             IN      NUMBER,
    p_mt_stat              IN      BOOLEAN,
    p_seal_stat            IN      BOOLEAN,
    p_curr_read_val        IN      NUMBER,
    p_curr_read_date       IN      DATE,
    p_prev_read            IN      ITEM%ROWTYPE,
    p_prev_act_read        IN      ITEM%ROWTYPE,
    p_new_kwh              IN OUT NUMBER,
    p_new_read_val         IN OUT NUMBER,
    p_new_read_type        IN OUT NUMBER,
    p_new_err_code         IN OUT NUMBER,
    p_force_zero_amnt      IN OUT BOOLEAN,
    p_est_reading          IN OUT NUMBER,
    p_force_est_correction IN OUT BOOLEAN
  );

  PROCEDURE calc_cycle_with_reading(
    p_cust_cat_key         IN     NUMBER,
    p_acc_inst_cp          IN     NUMBER,
    p_acc_cr_date          IN     DATE,
    p_is_cutted            IN     BOOLEAN,
    p_min_kwh              IN     NUMBER,
    p_max_kwh              IN     NUMBER,
    p_mt_type              IN     NUMBER,
    p_mt_digits            IN     NUMBER,
    p_mt_coeff             IN     NUMBER,
    p_mt_stat              IN     BOOLEAN,
    p_seal_stat            IN     BOOLEAN,
    p_curr_read_val        IN     NUMBER,
    p_curr_read_date       IN     DATE,
    p_prev_read            IN     ITEM%ROWTYPE,
    p_prev_act_read        IN     ITEM%ROWTYPE,
    p_new_kwh              IN OUT NUMBER,
    p_new_read_val         IN OUT NUMBER,
    p_new_read_type        IN OUT NUMBER,
    p_new_err_code         IN OUT NUMBER,
    p_force_zero_amnt      IN OUT BOOLEAN,
    p_est_reading          IN OUT NUMBER,
    p_force_est_correction IN OUT BOOLEAN
  );

  PROCEDURE calc_cycle_without_reading(
    p_acc_inst_cp          IN     NUMBER,
    p_acc_cr_date          IN     DATE,
    p_is_cutted            IN     BOOLEAN,
    p_min_kwh              IN     NUMBER,
    p_max_kwh              IN     NUMBER,
    p_mt_type              IN     NUMBER,
    p_mt_digits            IN     NUMBER,
    p_mt_coeff             IN     NUMBER,
    p_mt_stat              IN     BOOLEAN,
    p_seal_stat            IN     BOOLEAN,
    p_curr_read_date       IN     DATE,
    p_prev_read            IN     ITEM%ROWTYPE,
    p_prev_act_read        IN     ITEM%ROWTYPE,
    p_new_kwh              IN OUT NUMBER,
    p_new_read_val         IN OUT NUMBER,
    p_new_read_type        IN OUT NUMBER,
    p_new_err_code         IN OUT NUMBER,
    p_force_zero_amnt      IN OUT BOOLEAN,
    p_est_reading          IN OUT NUMBER,
    p_force_est_correction IN OUT BOOLEAN
  );

  PROCEDURE calc_not_cycle_reading (
    p_cust_cat_key         IN     NUMBER,
    p_acc_inst_cp          IN     NUMBER,
    p_acc_cr_date          IN     DATE,
    p_is_cutted            IN     BOOLEAN,
    p_mt_digits            IN     NUMBER,
    p_mt_coeff             IN     NUMBER,
    p_curr_read_val        IN     NUMBER,
    p_curr_read_type       IN     NUMBER,
    p_curr_read_date       IN     DATE,
    p_curr_kwh             IN     NUMBER,
    p_curr_amnt            IN     NUMBER,
    p_prev_read            IN     ITEM%ROWTYPE,
    p_prev_act_read        IN     ITEM%ROWTYPE,
    p_new_kwh              IN OUT NUMBER,
    p_new_read_val         IN OUT NUMBER,
    p_new_read_type        IN OUT NUMBER,
    p_force_zero_amnt      IN OUT BOOLEAN,
    p_est_reading          IN OUT NUMBER,
    p_force_est_correction IN OUT BOOLEAN
  );

  PROCEDURE process_estimate_correction (
    p_customer         IN     NUMBER,
    p_account          IN     NUMBER,
    p_schedule         IN     NUMBER,
    p_mt_digits        IN     NUMBER,
    p_mt_coeff         IN     NUMBER,
    p_new_read_val     IN     NUMBER,
    p_new_read_type    IN     NUMBER,
    p_new_read_date    IN     DATE,
    p_prev_actual_read IN     ITEM%ROWTYPE,
    p_prev_read        IN     ITEM%ROWTYPE,
    p_operator         IN     NUMBER,
    p_force_on_current IN OUT BOOLEAN,
    p_total_corrected  IN OUT NUMBER
  );

  PROCEDURE process_parent_correction (
    p_account         IN    NUMBER,
    p_processed_items IN    NUM_ARRAY,
    p_new_read_date   IN    DATE,
    p_operator        IN    NUMBER
  );

  PROCEDURE process_my_estimate_correction (
    p_customer         IN     NUMBER,
    p_account          IN     NUMBER,
    p_new_read_date    IN     DATE,
    p_schedule         IN     NUMBER,
    p_prev_actual_read IN     ITEM%ROWTYPE,
    p_prev_read        IN     ITEM%ROWTYPE,
    p_operator         IN     NUMBER,
    p_force_on_current IN OUT BOOLEAN,
    p_total_corrected  IN OUT NUMBER,
    p_processed_items     OUT NUM_ARRAY
  );

  PROCEDURE correct_estimate (
    est_item_key        IN    NUMBER,
    est_derived_reading IN    NUMBER,
    est_derived_kwh     IN    NUMBER
  );

  PROCEDURE correct_estimates (
    p_account        IN    NUMBER,
    p_new_reading    IN    NUMBER,
    p_new_readdate   IN    DATE,
    p_mt_digits      IN    NUMBER,
    p_mt_coeff       IN    NUMBER,
    p_last_real_read IN    ITEM%ROWTYPE,
    p_last_read      IN    ITEM%ROWTYPE
  );
  
  PROCEDURE calc_charge_for_cycle(
    p_cust_key        IN     NUMBER,
    p_cycle_key       IN     NUMBER,
    p_acc_key         IN     NUMBER,
    p_force_estimate  IN     BOOLEAN,
    p_total_kwh          OUT NUMBER,
    p_total_gel          OUT NUMBER
  );

  PROCEDURE calc_charge_for_cycle (
    p_cust_key        IN NUMBER,
    p_cycle_key       IN NUMBER,
    p_acc_key         IN NUMBER,
    p_force_est       IN BOOLEAN,
    p_total_kwh          OUT NUMBER,
    p_total_gel          OUT NUMBER,
    p_total_kwh_psubs    OUT NUMBER,
    p_total_gel_psubs    OUT NUMBER,
    p_add_kwh_add        OUT NUMBER,
    p_add_gel_add        OUT NUMBER,
    p_processed_items IN OUT NUM_ARRAY,
    p_details         IN OUT tp_item_details,
    p_add_details     IN OUT tp_item_details
  );

  PROCEDURE calc_complex_cycle (
    p_cust_key        IN     NUMBER,
    p_cycle_key       IN     NUMBER,
    p_acc_key         IN     NUMBER,
    p_force_est       IN     BOOLEAN,
    p_total_kwh       IN OUT NUMBER,
    p_total_gel       IN OUT NUMBER,
    p_total_kwh_psubs IN OUT NUMBER,
    p_total_gel_psubs IN OUT NUMBER,
    p_add_kwh_add        OUT NUMBER,
    p_add_gel_add        OUT NUMBER,
    p_processed_items IN OUT NUM_ARRAY,
    p_details         IN OUT tp_item_details
  );

  PROCEDURE charge_for_parent (
    p_from_item IN     NUMBER,
    p_kwh_old      OUT NUMBER,
    p_kwh_new      OUT NUMBER,
    p_gel_old      OUT NUMBER,
    p_gel_new      OUT NUMBER,
    p_year         OUT NUMBER,
    p_prnt_acc     OUT NUMBER,
    p_prnt_cust    OUT NUMBER
  );

  PROCEDURE calc_simple_est_cycle (
    p_cycle_key       IN     NUMBER,
    p_acc_key         IN     NUMBER,
    p_force_est       IN     BOOLEAN,
    p_total_kwh       IN OUT NUMBER,
    p_total_gel       IN OUT NUMBER,
    p_total_kwh_psubs IN OUT NUMBER,
    p_total_gel_psubs IN OUT NUMBER,
    p_add_kwh_add        OUT NUMBER,
    p_add_gel_add        OUT NUMBER,
    p_processed_items IN OUT NUM_ARRAY,
    p_details         IN OUT tp_item_details,
    p_add_details     IN OUT tp_item_details
  );

  PROCEDURE compare_cycle_charges (
    p_acc_key          IN     NUMBER,
    p_read_date        IN     DATE,
    p_prev_read        IN     ITEM%ROWTYPE,
    p_prev_actual_read IN     ITEM%ROWTYPE,
    p_corrections         OUT tp_corrections,
    p_processed_items     OUT NUM_ARRAY
  );

  FUNCTION populate_item (
    p_customer         IN    NUMBER,
    p_account          IN    NUMBER,
    p_tariff           IN    NUMBER,
    p_schedule         IN    NUMBER,
    p_bill_operation   IN    NUMBER,
    p_operator         IN    NUMBER,
    p_sign_person      IN    NUMBER,
    p_item_date        IN    DATE,
    p_item_number      IN    VARCHAR2,
    p_reading          IN    NUMBER,
    p_kwh              IN    NUMBER,
    p_gel              IN    NUMBER,
    p_item_cat         IN    NUMBER,
    p_note             IN    NUMBER,
    p_record_category  IN    NUMBER,
    p_from_item        IN    NUMBER,
    p_child            IN    NUMBER,
    p_child_cycle      IN    NUMBER,
    p_details          IN    tp_item_details := NULL
  ) RETURN NUMBER;
  
  PROCEDURE add_item_details (
    p_item_key NUMBER,
    p_details tp_item_details
  );

  PROCEDURE populate_route_store (
    p_final_err_code  IN    NUMBER,
    p_final_kwh       IN    NUMBER,
    p_final_read_type IN    NUMBER,
    p_route_store_rec IN    route_store%ROWTYPE,
    p_final_read_val  IN    NUMBER,
    p_send_to_print   IN    BOOLEAN,
    p_full_kwh        IN    NUMBER := NULL
  );

  PROCEDURE process_parent (
    p_parent         IN    tp_parent,
    p_item_from      IN    NUMBER,
    p_child_account  IN    NUMBER,
    p_child_kwh      IN    NUMBER,
    p_child_schedule IN    NUMBER,
    p_initial_date   IN    DATE,
    p_final_date     IN    DATE,
    p_operator       IN    NUMBER
  );

  PROCEDURE process_percent_add_charge (
    p_customer  IN     NUMBER,
    p_account   IN     NUMBER,
    p_kwh       IN     NUMBER,
    p_gel       IN     NUMBER,
    p_schedule  IN     NUMBER,
    p_item_date IN     DATE,
    p_from_item IN     NUMBER,
    p_operator  IN     NUMBER,
    p_item_key     OUT NUMBER,
    p_add_kwh      OUT NUMBER,
    p_add_gel      OUT NUMBER
  );

  PROCEDURE process_kwh_add_charge (
    p_customer   IN     NUMBER,
    p_account    IN     NUMBER,
    p_schedule   IN     NUMBER,
    p_init_date  IN     DATE,
    p_final_date IN     DATE,
    p_operator   IN     NUMBER,
    p_has_flat   IN     BOOLEAN,
    p_item_key      OUT NUMBER,
    p_add_kwh       OUT NUMBER,
    p_add_gel       OUT NUMBER
  );

  PROCEDURE process_kwh_subsidy (
    p_customer        IN     NUMBER,
    p_account         IN     NUMBER,
    p_schedule        IN     NUMBER,
    p_init_date       IN     DATE,
    p_final_date      IN     DATE,
    p_operator        IN     NUMBER,
    p_has_flat        IN     BOOLEAN,
    p_item_key           OUT NUMBER,
    p_add_kwh            OUT NUMBER,
    p_add_gel            OUT NUMBER
  );

  PROCEDURE process_percent_subsidy (
    p_customer       IN     NUMBER,
    p_account        IN     NUMBER,
    p_kwh            IN     NUMBER,
    p_gel            IN     NUMBER,
    p_schedule       IN     NUMBER,
    p_item_date      IN     DATE,
    p_from_item      IN     NUMBER,
    p_operator       IN     NUMBER,
    p_item_key          OUT NUMBER,
    p_subs_kwh          OUT NUMBER,
    p_subs_gel          OUT NUMBER
  );

  PROCEDURE process_gel_charges (
    p_customer       IN     NUMBER,
    p_account        IN     NUMBER,
    p_schedule       IN     NUMBER,
    p_item_date      IN     DATE,
    p_operator       IN     NUMBER
  );

  PROCEDURE process_percent_subsidy_parent (
    p_customer       IN     NUMBER,
    p_account        IN     NUMBER,
    p_kwh            IN     NUMBER,
    p_gel            IN     NUMBER,
    p_schedule       IN     NUMBER,
    p_item_date      IN     DATE,
    p_from_item      IN     NUMBER,
    p_operator       IN     NUMBER,
    p_child          IN     NUMBER,
    p_child_cycle    IN     NUMBER
  );

  FUNCTION get_tariff_for_date (
    p_account         IN     NUMBER,
    p_some_date       IN     DATE   := SYSDATE,
    p_operation       IN     NUMBER := OPER_READING
  ) RETURN NUMBER;

  FUNCTION has_step_tariff (
     p_account        IN     NUMBER,
     p_some_date      IN     DATE   := SYSDATE,
     p_operation      IN     NUMBER := OPER_READING
  ) RETURN BOOLEAN;

  FUNCTION has_step_tariff (
     p_account   NUMBER,
     p_d1        DATE,
     p_d2        DATE,
     p_operation NUMBER := OPER_READING
  ) RETURN BOOLEAN;

  FUNCTION calc_gel_for_natia (
    p_account         IN     NUMBER,
    p_charged_kwh     IN     NUMBER,
    p_charge_period   IN     NUMBER
  ) RETURN NUMBER;

  FUNCTION calc_gel (
    p_tar_key         IN     NUMBER,
    p_charged_kwh     IN     NUMBER,
    p_charge_period   IN     NUMBER,
    p_d2              IN     DATE
  ) RETURN NUMBER;

  FUNCTION calc_gel (
    p_tar_key         IN     NUMBER,
    p_charged_kwh     IN     NUMBER,
    p_charge_period   IN     NUMBER,
    p_d2              IN     DATE,
    p_force_flat      IN     BOOLEAN := FALSE,
    p_details         IN OUT tp_item_details,
    p_negative        IN     BOOLEAN := FALSE
  ) RETURN NUMBER;

  FUNCTION calc_gel (
    p_tar_flat        IN     tarcomp%ROWTYPE,
    p_tar_steps       IN     tp_tar_steps,
    p_charged_kwh     IN     NUMBER,
    p_charge_period   IN     NUMBER,
    p_d2              IN     DATE,
    p_force_flat      IN     BOOLEAN := FALSE,
    p_details         IN OUT tp_item_details,
    p_negative        IN     BOOLEAN := FALSE
  ) RETURN NUMBER;

  FUNCTION calc_gel (
    pacckey       IN     NUMBER,
    pbilloper     IN     NUMBER,
    pstartdate    IN     DATE,
    penddate      IN     DATE,
    pchargedkwh   IN     NUMBER,
    pforceflat    IN     BOOLEAN,
    pdetails      IN OUT tp_item_details,
    p_negative    IN     BOOLEAN := FALSE
  ) RETURN NUMBER;

  FUNCTION calc_gel_for_item (
    p_acc_key         IN     NUMBER,
    p_tar_acc_key     IN     NUMBER,
    p_item            IN     NUMBER,
    p_kwh             IN     NUMBER,
    p_acc_creation    IN     DATE,
    p_from_last_cycle IN     BOOLEAN := FALSE,
    p_negative        IN     BOOLEAN := FALSE,
    p_details         IN OUT tp_item_details
  ) RETURN NUMBER;

  FUNCTION get_oper_cat (
    p_oper_key IN     NUMBER
  ) RETURN NUMBER;

  FUNCTION derive_est_kwh (
    p_init_date IN     DATE,
    p_fin_date  IN     DATE,
    p_inst_cp   IN     NUMBER
  ) RETURN NUMBER;

  FUNCTION derive_read_val (
    p_read1       IN     NUMBER,
    p_read2       IN     NUMBER,
    p_date1       IN     DATE,
    p_date2       IN     DATE,
    p_edate       IN     DATE,
    p_mt_digits   IN     NUMBER
  ) RETURN NUMBER;

  FUNCTION derive_read_val (
    p_iread         IN     NUMBER,
    p_charged_kwh   IN     NUMBER,
    p_coeff         IN     NUMBER,
    p_digits        IN     NUMBER
  ) RETURN NUMBER;

  FUNCTION derive_kwh (
    curr_r        IN     NUMBER,
    prev_r        IN     NUMBER,
    cont_r        IN     NUMBER,
    coeff         IN     NUMBER,
    digit         IN     NUMBER,
    p_force_exact IN     BOOLEAN := FALSE
  ) RETURN NUMBER;

  FUNCTION get_cycle(
    p_sched_key IN     NUMBER
  ) RETURN NUMBER;
  
  FUNCTION get_1st_not_cycle_R_for_cycle (
    p_account NUMBER,
    p_schedule NUMBER
  ) RETURN ITEM%ROWTYPE;

  PROCEDURE get_previous_cycle_info (
    p_account NUMBER,
    p_schedule NUMBER,
    p_prev_cycle_last_item OUT NUMBER,
    p_prev_cycle_last_itemdate OUT DATE,
    p_prev_cycle_last_enterdate OUT DATE
  );

  FUNCTION get_schedule (
   p_acc_key   IN     NUMBER,
   p_cycle_key IN     NUMBER
  ) RETURN NUMBER;

  FUNCTION get_last_read (
    p_acc_key     IN     NUMBER,
    p_max_date    IN     DATE,
    p_real_read   IN     BOOLEAN := FALSE
  ) RETURN item%ROWTYPE;

  FUNCTION get_tar_comp_step (
    p_tar_key IN     NUMBER
  ) RETURN tp_tar_steps;

  FUNCTION get_tar_comp_flat (
    p_tar_key IN     NUMBER
  ) RETURN tarcomp%ROWTYPE;

  FUNCTION get_parent_account(
    p_account IN     NUMBER
  ) RETURN tp_parent;

  PROCEDURE clear_warnings;

  PROCEDURE submit_error (
    err_code IN     NUMBER,
    err_text IN     VARCHAR2
  );

  PROCEDURE submit_warning (
    err_code NUMBER,
    err_text VARCHAR2
  );

  PROCEDURE get_acc_info (
    p_acc_key      IN     NUMBER,
    p_acc_cr_date     OUT DATE,
    p_inst_cp         OUT NUMBER,
    p_is_cutted       OUT BOOLEAN,
    p_cust_key        OUT NUMBER,
    p_cust_cat_key    OUT NUMBER,
    p_mtt_type        OUT NUMBER,
    p_mtt_digits      OUT NUMBER,
    p_mtt_coeff       OUT NUMBER,
    p_min_kwh         OUT NUMBER,
    p_max_kwh         OUT NUMBER,
    p_balance         OUT NUMBER,
    p_is_main_acc     OUT BOOLEAN,
    p_is_closed       OUT BOOLEAN
  );

  FUNCTION get_accounts (
    p_cust_key IN     NUMBER,
    p_active   IN     BOOLEAN := FALSE
  ) RETURN NUM_ARRAY;

  FUNCTION get_job (
    p_oper_key IN     NUMBER
  ) RETURN NUMBER;

  FUNCTION get_correction_code (
    p_year IN     NUMBER,
    p_type IN     NUMBER
  ) RETURN NUMBER;
  
  FUNCTION get_cycle_start_point (
    p_end_point                 IN     DATE,
    p_account                   IN     NUMBER,
    p_schedule                  IN     NUMBER,
    p_acc_creation              IN     DATE,
    p_prev_cycle_last_enterdate    OUT DATE,
    p_prev_cycle_last_item         OUT NUMBER
  ) RETURN DATE;

  FUNCTION get_cycle_initial_date (
    p_account  IN     NUMBER,
    p_schedule IN     NUMBER
  ) RETURN DATE;

  FUNCTION get_cycle_final_date (
    p_schedule IN     NUMBER
  ) RETURN DATE;

  PROCEDURE expand_item_details (
    new_details IN     tp_item_details,
    all_details IN OUT tp_item_details
  );

  PROCEDURE expand_item_details (
    new_detail  IN     tp_item_detail,
    all_details IN OUT tp_item_details
  );

END; -- end of the package specification
/

create or replace PACKAGE BODY    BILL_MANAGER_2006
IS
   /* TEMPORARY PROCEDURE */
   PROCEDURE close_cycle_temp (p_route_store_key NUMBER)
   IS
      p_route_store_rec   route_store%ROWTYPE;
   BEGIN
      -- clear warnings
      clear_warnings;

      -- get routes
      SELECT *
        INTO p_route_store_rec
        FROM route_store
       WHERE rtstorekey = p_route_store_key;

      -- process cycle reading for this account
      new_cycle_reading (p_route_store_rec, TRUE, 1, 1);
      -- process debt agreement
      process_debt_agreement (p_route_store_rec);
--    -- delete this record from the route store
--    --delete from route_store where RTSTOREKEY = p_route_store_key;

      -- do not commit
      --RAISE_APPLICATION_ERROR(-20000, 'exit proc with rollback');

      -- commit changes
      COMMIT;
   -- exception details
   EXCEPTION
      WHEN OTHERS
      THEN
         DBMS_OUTPUT.put_line (SQLERRM);
         ROLLBACK;
         submit_error (SQLCODE, SQLERRM);
   END;                               -- end of the 'close_cycle_temp' procedure

   /**
    * Recalculate last cycle.
    */
   PROCEDURE recalculate_last_cycle (
      p_account        NUMBER,
      p_new_reading    NUMBER,
      p_oper_key       NUMBER,
      p_force_recalc   BOOLEAN := FALSE
   )
   IS
      p_item_date          DATE;
      p_current_schedule   NUMBER;
      p_counter            NUMBER;
      p_last_item          NUMBER;
      p_first_item         NUMBER;
      p_schedule           NUMBER;
      p_cycle_date         DATE;
      p_printed_count      NUMBER                := 0;
      p_force              BOOLEAN               := NVL (p_force_recalc, FALSE);
      p_rs_key             NUMBER;
      p_route_store_rec    route_store%ROWTYPE;
      p_parent_item_key    NUMBER;
      p_last_non_cycle     NUMBER;
      p_amount             NUMBER;
      p_cycle_item         NUMBER;
      p_parent             tp_parent;
   BEGIN                      -- begin of the 'recalculate_last_cycle' procedure
      -- clear warnings
      clear_warnings;

      -- Look up for the appropriate route store key

      -- try to get the route store key
      BEGIN
         SELECT rtstorekey, schedkey
           INTO p_rs_key, p_current_schedule
           FROM route_store
          WHERE acckey = p_account AND status != 2;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            raise_application_error (-20000,
                                        'Can not find record in the '
                                     || 'RouteStore table for this account.'
                                    );
      END;

      -- Gather information about this cycle

      -- get cycle date
      SELECT itemdate
        INTO p_item_date
        FROM item
       WHERE acckey = p_account AND schedkey = p_current_schedule AND ROWNUM = 1;

      -- get last item for this account
      SELECT /*+ FIRST_ROW */
             itemkey, schedkey, itemdate
        INTO p_last_item, p_schedule, p_cycle_date
        FROM (SELECT   itemkey, schedkey, itemdate
                  FROM item
                 WHERE acckey = p_account
                   AND itemdate = p_item_date
                   AND schedkey IS NOT NULL
                   -- ignore accumulated subsidies!!!
                   AND billoperkey NOT IN (515, 516, 517)
              ORDER BY itemkey DESC)
       WHERE ROWNUM = 1;

      IF p_schedule != p_current_schedule
      THEN
         raise_application_error
                          (-20000,
                              'Can not recalculate last cycle: '
                           || 'Last cycle and RouteStore data are not compatible.'
                          );
      END IF;

      -- try to find operation in another date
      SELECT COUNT (itemkey)
        INTO p_counter
        FROM item
       WHERE acckey = p_account
         AND itemdate > p_item_date
         -- ignore accumulated subsidies!!!
         AND billoperkey NOT IN (515, 516, 517);

      IF p_counter > 0
      THEN
         raise_application_error (-20000,
                                     'Can not recalculate last cycle: '
                                  || 'other operations for different dates.'
                                 );
      END IF;

      -- try to analyze parent
      p_parent := get_parent_account (p_account);

      IF p_parent.ACCOUNT IS NOT NULL
      THEN
         -- try to find operation in another date
         SELECT COUNT (itemkey)
           INTO p_counter
           FROM item
          WHERE acckey = p_parent.ACCOUNT AND itemdate > p_item_date;

         IF p_counter > 0
         THEN
            raise_application_error
                   (-20000,
                       'Can not recalculate last cycle: '
                    || 'other operations for different dates (in parent account).'
                   );
         END IF;
      END IF;

      -- Look up while customer is already printed

      -- check this only when recalculation is not forced
      IF NOT p_force
      THEN
         -- get count of item category = 1
         SELECT COUNT (*)
           INTO p_printed_count
           FROM item
          WHERE acckey = p_account AND schedkey = p_schedule AND itemcatkey = 1;

         -- when printed raise error
         IF NVL (p_printed_count, 0) > 0
         THEN
            raise_application_error (-20000,
                                        'Can not recalculate last cycle: '
                                     || 'customer is already printed.'
                                    );
         END IF;
      END IF;                                     -- end of the forced condition

      -- Delete this cycle

      -- loop over all records
      FOR rec IN (SELECT i.*
                    FROM item i, billoperation bop
                   WHERE i.billoperkey = bop.billoperkey
                     AND i.acckey = p_account
                     AND i.schedkey = p_schedule
                     AND i.billoperkey NOT IN
                            (oper_debt_reschedule,
                             oper_debt_reschedule_percent,
                             oper_parent_charge,
                             oper_debt_activation,
                             oper_debt_deactivation
                            ))
      LOOP
         -- cycle item
         p_cycle_item := rec.itemkey;
         -- delete all readings and parent charges -- do not delete percent charges
         delete_item_and_related (rec.itemkey, FALSE, TRUE);
      END LOOP;

      -- Delete accumulated subsidy

      -- plain delete
      FOR rec IN (SELECT i.*
                    FROM item i
                   WHERE i.acckey = p_account
                     AND i.itemkey > p_cycle_item
                     AND (   i.billoperkey = 515
                          OR i.billoperkey = 516
                          OR i.billoperkey = 517
                         ))
      LOOP
         delete_item_and_related (rec.itemkey, FALSE, TRUE);
      END LOOP;

      -- triming
      accumulable_subsidy_2006.trim_diff_subsidy (p_account);

      -- Update reading information

      -- update ROUTE_STORE
      UPDATE route_store
         SET new_reading = p_new_reading
       WHERE rtstorekey = p_rs_key;

      -- Process new cycle reading using route store record

      -- get route store record
      SELECT *
        INTO p_route_store_rec
        FROM route_store
       WHERE rtstorekey = p_rs_key;

      -- process cycle reading for this account
      new_cycle_reading (p_route_store_rec, TRUE, p_oper_key, p_oper_key);
      accumulable_subsidy_2006.run_diffsubs_accumulation_2006
                              (p_route_store_rec.blockkey,
                               get_cycle_final_date (p_route_store_rec.schedkey),
                               p_oper_key
                              );
      -- commit changes
      COMMIT;
    -- exception details
--    EXCEPTION WHEN OTHERS
--    THEN

   --      ROLLBACK;
--      submit_error(SQLCODE, SQLERRM);
   END;                         -- end of the procedure 'recalculate_last_cycle'

   PROCEDURE delete_item_and_related (p_item IN NUMBER)
   IS
   BEGIN
      delete_item_and_related (p_item, TRUE, TRUE);
   EXCEPTION
      WHEN OTHERS
      THEN
         ROLLBACK;
         submit_error (SQLCODE, SQLERRM);
   END;                        -- end of the 'delete_item_and_related' procedure

   /**
    * Deletes item and all records related to it.
    */
   PROCEDURE delete_item_and_related (
      p_item              NUMBER,
      p_delete_percents   BOOLEAN,
      p_delete_parent     BOOLEAN
   )
   IS
      p_parent_item_key    NUMBER;
      p_percent_item_key   NUMBER;
      p_account            NUMBER;
      p_customer           NUMBER;
      p_parent             NUMBER;
      p_parent_date        DATE;
   BEGIN
      -- get account and balance for this item
      SELECT custkey, acckey
        INTO p_customer, p_account
        FROM item
       WHERE itemkey = p_item;

      -- delete parent information
      IF p_delete_parent
      THEN
         BEGIN
            -- search for parent record
            SELECT ir_item
              INTO p_parent_item_key
              FROM item_relation
             WHERE ir_item_from = p_item AND ir_type = item_rec_parent_charge;

            -- Get parent account ID
            SELECT acckey, itemdate
              INTO p_parent, p_parent_date
              FROM item
             WHERE itemkey = p_parent_item_key;

            -- TODO: not so clear operation!
            -- delete system vouchers for this date
            DELETE FROM item
                  WHERE acckey = p_parent
                    AND itemdate = p_parent_date
                    AND itemnumber = 'prnt' || p_account;

            -- call this procedure for % charges deletion
            delete_item_and_related (p_parent_item_key, TRUE, FALSE);

            -- delete parent % charge
            DELETE FROM item
                  WHERE itemkey = p_parent_item_key;
         -- do nothing when data can not be found
         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
               NULL;
         END;
      END IF;

      -- delete percent charges
      IF p_delete_percents
      THEN
         BEGIN
            -- search parent % subsidy
            SELECT ir_item
              INTO p_percent_item_key
              FROM item_relation
             WHERE ir_item_from = p_item
               AND ir_type IN
                      (item_rec_percent_charge, item_rec_parent_percent_charge);

            -- call this procedure for parent charges deletion
            delete_item_and_related (p_percent_item_key, FALSE, TRUE);
         -- do nothing when data can not be found
         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
               NULL;
         END;
      END IF;

      -- delete current record
      DELETE FROM item
            WHERE itemkey = p_item;

      -- update balances
      update_balances_history (p_customer, p_item);
   END;                        -- end of the 'delete_item_and_related' procedure

   PROCEDURE update_balances_history (p_customer NUMBER, p_start_item NUMBER)
   IS
      p_balance             NUMBER;
      p_prev_item           NUMBER;
      p_accounts            num_array      := num_array ();
      p_last_real_reading   item%ROWTYPE;
      p_index               NUMBER         := 1;
      p_start_account       NUMBER;
      p_prev_enterdate      DATE;
   BEGIN
      -- try to get previous balance
      BEGIN
         SELECT balance, itemkey, acckey, enterdate
           INTO p_balance, p_prev_item, p_start_account, p_prev_enterdate
           FROM (SELECT   balance, itemkey, acckey, enterdate
                     FROM item
                    WHERE custkey = p_customer
                      AND itemkey < p_start_item
                      AND
                          -- @since 11/07/2006 cut on ENTERDATE to improve performace
                          enterdate > item_cut_date
                 ORDER BY itemkey DESC)
          WHERE ROWNUM = 1;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            BEGIN
               SELECT balance, itemkey, acckey, enterdate
                 INTO p_balance, p_prev_item, p_start_account, p_prev_enterdate
                 FROM (SELECT   balance, itemkey, acckey, enterdate
                           FROM item
                          WHERE custkey = p_customer AND itemkey < p_start_item
                       ORDER BY itemkey DESC)
                WHERE ROWNUM = 1;
            EXCEPTION
               WHEN NO_DATA_FOUND
               THEN
                  -- no previous history!
                  p_balance := 0;
                  p_prev_item := 0;
                  p_prev_enterdate := infinity_past;
            END;
      END;

      p_accounts.EXTEND;
      p_accounts (p_accounts.COUNT) := NVL (p_start_account, 0);

-- @since 13/Jan/2010: ZVIADI: place data into temporary table for faster looping

      delete item_l_t;

      -- Update balances for the next items
      FOR bop IN (SELECT billoperkey, OPERTPKEY FROM billoperation) 
      LOOP
        FOR rec IN (SELECT i.itemkey, i.acckey, i.amount
                      FROM item i
                     WHERE bop.billoperkey = i.billoperkey
                       AND custkey = p_customer
                       AND itemkey >= p_prev_item
                       AND
                           -- @since 06/07/2006: cut on ENTERDATE to improve performance
                           enterdate > p_prev_enterdate - 30
                  ORDER BY itemkey)
        LOOP
          INSERT INTO item_l_t (ITEMKEY, ACCKEY, AMOUNT, OPERTPKEY)
                        VALUES (rec.itemkey, rec.acckey, rec.amount, bop.OPERTPKEY);
        END LOOP;
      END LOOP;

/**
      -- Update balances for the next items
      FOR rec IN (SELECT   i.*, bop.opertpkey
                      FROM item i, billoperation bop
                     WHERE bop.billoperkey = i.billoperkey
                       AND custkey = p_customer
                       AND itemkey >= p_prev_item
                       AND
                           -- @since 06/07/2006: cut on ENTERDATE to improve performance
                           enterdate > p_prev_enterdate - 30
                  ORDER BY itemkey)
*/
      FOR rec IN (SELECT * FROM item_l_t ORDER BY itemkey)
      loop

-- @end @since 13/Jan/2010

         -- Add processed items
         IF NOT p_accounts.EXISTS (rec.acckey)
         THEN
            p_accounts.EXTEND;
            p_accounts (p_accounts.COUNT) := rec.acckey;
         END IF;

         -- Ignore previous item
         IF rec.itemkey != p_prev_item
         THEN
            -- update current balance
            UPDATE item
               SET balance = p_balance
             WHERE itemkey = rec.itemkey;
         END IF;

         -- update balance
         IF rec.opertpkey IS NULL
         THEN
            raise_application_error (-20000, 'Type is not defined');
         ELSIF rec.opertpkey = oper_cat_payment
         THEN
            p_balance := p_balance - NVL (rec.amount, 0);
         ELSE
            p_balance := p_balance + NVL (rec.amount, 0);
         END IF;
      END LOOP;

      -- Update last real readings for all processed accounts
      WHILE p_index < p_accounts.COUNT
      LOOP
         -- Get real reading
         p_last_real_reading.acckey := NULL;
         p_last_real_reading :=
                               get_last_read (p_accounts (p_index), NULL, TRUE);

         -- Update reading
         IF p_last_real_reading.acckey IS NULL
         THEN
            UPDATE ACCOUNT
               SET last_reading = 0.0,
                   last_read_date = NULL
             WHERE acckey = p_accounts (p_index);
         ELSE
            UPDATE ACCOUNT
               SET last_reading = p_last_real_reading.reading,
                   last_read_date = p_last_real_reading.itemdate
             WHERE acckey = p_accounts (p_index);
         END IF;

         -- increase index
         p_index := p_index + 1;
      END LOOP;

      -- Update customer balance
      UPDATE customer
         SET balance = p_balance
       WHERE custkey = p_customer;
   END;                                -- end of the 'update_balances' procedure

   /**
    * This procedure analizes readings entered by the operator and indicates
    * exception fields. It also derives kWh charge for the current cycle. This
    * may be not only record for the account after cycle closing.
    * <p>
    *
    * This procedure mainly uses 'calc_kwh' procedure from this package.
    * <p>
    *
    * GUI form directly calls this procedure. When exception occures, system
    * sends exception message into temporary error table, which can be then
    * retrived by the form and message displayed for the user.
    * <p>
    *
    * @param p_sched_key    schedule key, which identifies the cycle of the route
    * @param p_close_route  close or not the route?
    * @param p_insp_key     inspector key
    * @param p_oper_key     operator key
    * @param p_oper_job     operator job key
    * @param is_power_user  is operator a power user?
    */
   PROCEDURE calc_route (
      p_sched_key     IN   NUMBER,
      p_close_route   IN   BOOLEAN,
      p_insp_key      IN   NUMBER,
      p_oper_key      IN   NUMBER,
      p_oper_job      IN   NUMBER,
      is_power_user   IN   BOOLEAN
   )
   IS
      -- cursor on BS.ROUTE_STORE records for the given schedule
      CURSOR sched_records
      IS
         (SELECT r.*
            FROM route_store r
           WHERE r.schedkey = p_sched_key);

      -- schedule status
      p_sched_stat       NUMBER;
      -- update for the schedule status
      p_sched_new_stat   NUMBER;
   BEGIN                                  -- begin of the 'calc_route' procedure
      -- Clear previous warnings
      clear_warnings;

      -- Check job of the operator
      IF p_oper_job IS NULL OR p_oper_job NOT IN (1, 2, 3, 6)
      THEN
         raise_application_error (-20000, 'User has not enough privilegies');
      END IF;

      -- Get schedule status
      SELECT     stat
            INTO p_sched_stat
            FROM schedule
           WHERE schedkey = p_sched_key
      FOR UPDATE;

      -- Delete temporary results of the previous calculations
      DELETE      route_store
            WHERE schedkey = p_sched_key AND status = 2;

      -- Update 'exceptions'
      UPDATE route_store
         SET status = 0,
             ERROR_CODE = NULL
       WHERE schedkey = p_sched_key AND status = 1;

      -- Iterate over all schedule records
      FOR rec IN sched_records
      LOOP
         -- Manage single record
         new_cycle_reading (rec, FALSE, p_oper_key, p_insp_key);
      END LOOP;

      -- Udate schedule status, when route is closed
      IF p_close_route IS NOT NULL AND p_close_route
      THEN
         -- Derive schedule new status
         IF is_power_user
         THEN
            p_sched_new_stat := 4;
         ELSE
            p_sched_new_stat := 2;
         END IF;

         -- Update schedule status
         UPDATE schedule
            SET OPERATOR = p_oper_key,
                stat = p_sched_new_stat,
                inspector = p_insp_key,
                enterdate = SYSDATE
          WHERE schedkey = p_sched_key;
      END IF;                                          -- end of schedule update

      -- Commit changes
      COMMIT;
   -- Manage exceptions
   EXCEPTION
      WHEN OTHERS
      THEN
         -- Rollback transactions
         ROLLBACK;

         -- Set schedule status to '0'
         UPDATE schedule
            SET OPERATOR = p_oper_key,
                stat = 0,
                inspector = p_insp_key,
                enterdate = SYSDATE
          WHERE schedkey = p_sched_key;

         -- Commit new changes
         COMMIT;
         -- Submit errors to the user interface
         submit_error (SQLCODE, SQLERRM);
   END;                                         -- end of 'calc_route' procedure

   /**
    * This procedure calculates block (set of rotes). It performes calculations
    * for all accounts in block and then sends them for final cycle closing.
    * Many other charges may be processed compare what was viewd by the
    * 'calc_route' procedure.
    * <p>
    *
    * GUI form directly calls this procedure. When exception occures, system
    * sends exception message into temporary error table, which can be then
    * retrived by the form and message displayed for the user.
    * <p>
    *
    * @param p_block_key    block key
    * @param p_cycle_date   cycle date for this block
    * @param p_oper_key     operator key, which performes block calculations
    * @param p_insp_key     inspector key
    */
   PROCEDURE calc_block (
      p_block_key    IN   NUMBER,
      p_cycle_date   IN   DATE,
      p_oper_key     IN   NUMBER,
      p_insp_key     IN   NUMBER
   )
   IS
      -- cycle date numeric representation
      p_cycle_date_j           NUMBER
                                     := TO_NUMBER (TO_CHAR (p_cycle_date, 'J'));

      -- block cursor
      CURSOR p_block_cursor
      IS
         SELECT   rs.*
             FROM route_store rs, ACCOUNT a
            WHERE blockkey = p_block_key
              AND TO_NUMBER (TO_CHAR (new_readdate, 'J')) = p_cycle_date_j
              AND a.acckey = rs.acckey
         ORDER BY a.mainaccount DESC;

      -- operator job
      p_job                    NUMBER          := get_job (p_oper_key);
      -- is power user?
      is_power_user            BOOLEAN
                                     := p_job IS NOT NULL AND p_job IN
                                                                      (2, 3, 6);
      -- checking parameters
      p_feature_schedules      NUMBER;
      p_not_processed_routes   NUMBER;
      p_processed_routes       NUMBER;
      -- Print key
      p_for_print_key          NUMBER;
      -- Current Account
      p_curr_account           NVARCHAR2 (100);
      p_item_key               NUMBER;
      p_details                tp_item_details;
   BEGIN                                  -- begin of the 'calc_block' procedure
      -- Clear previous warnings
      clear_warnings;

      -- Check user privilegies

      -- only administrators has privelegy call this procedure
      IF NOT is_power_user
      THEN
         raise_application_error (-20000, 'User has not enough privilegies');
      END IF;

      -- Check current calculation conditions

      -- count schedules that have been already completed
      -- and cycle date is greater than current cycle date
      SELECT COUNT (*)
        INTO p_feature_schedules
        FROM route r, schedule s
       WHERE r.blockkey = p_block_key
         AND r.routekey = s.routekey
         AND TO_NUMBER (TO_CHAR (s.cycledate, 'J')) > p_cycle_date_j
         AND s.stat = 5;

      -- some schedules are completed
      IF p_feature_schedules IS NOT NULL AND p_feature_schedules > 0
      THEN
         raise_application_error (-20000,
                                  'Schedule is too old for this operation'
                                 );
      END IF;

      -- count routes from current block that doesn't have
      -- status 'Q/A is complete'
      SELECT COUNT (*)
        INTO p_not_processed_routes
        FROM route r, schedule s
       WHERE r.blockkey = p_block_key
         AND r.routekey = s.routekey
         AND TO_NUMBER (TO_CHAR (s.cycledate, 'J')) = p_cycle_date_j
         AND s.stat < 4;

      -- some routes are not proccessed
      IF p_not_processed_routes IS NOT NULL AND p_not_processed_routes > 0
      THEN
         raise_application_error (-20000,
                                  'Block is not ready for this operation'
                                 );
      END IF;

      -- count routes from current block with status 'Route is ready to print'
      SELECT COUNT (*)
        INTO p_processed_routes
        FROM route r, schedule s
       WHERE r.blockkey = p_block_key
         AND r.routekey = s.routekey
         AND TO_NUMBER (TO_CHAR (s.cycledate, 'J')) = p_cycle_date_j
         AND s.stat = 5;

      -- block is sent to print
      IF p_processed_routes > 0
      THEN
         raise_application_error (-20000, 'Block is already sent to print');
      END IF;

      -- update and lock shedule rows
      UPDATE schedule
         SET OPERATOR = p_oper_key,
             stat = 5,
             enterdate = SYSDATE
       WHERE schedkey IN (
                SELECT s.schedkey
                  FROM route r, schedule s
                 WHERE r.blockkey = p_block_key
                   AND r.routekey = s.routekey
                   AND TO_NUMBER (TO_CHAR (s.cycledate, 'J')) = p_cycle_date_j
                   AND s.stat = 4);

      -- Proccess block calculations

      -- First will be proccesed NOT main accounts: see p_block_cursor definition
      FOR rec IN p_block_cursor
      LOOP
         -- ignore additional rows from exception situations
         IF rec.status != 2
         THEN
            p_curr_account := rec.accid;
            -- process cycle reading for this account
            new_cycle_reading (rec, TRUE, p_oper_key, p_insp_key);



            -- @since 10/04/2007: update current meter status for account
            UPDATE ACCOUNT
               SET
                mtcondit = 
                    CASE WHEN rec.new_mtstat = 1 THEN rec.new_mtstat
                    ELSE rec.cur_mtstat END,
                mtslcond =
                    CASE WHEN rec.new_sealstat = 1 THEN rec.new_sealstat
                    ELSE rec.cur_sealstat END
             WHERE acckey = rec.acckey;

            -- process debt agreement
            process_debt_agreement (rec);

            -- close debt-agreement when left amount is too small
            FOR debtrec IN (SELECT *
                              FROM debt_agreement
                             WHERE status = 0 AND acckey = rec.custkey)
            LOOP
               IF ABS (NVL (debtrec.left_amount, 0)) <= min_debt
               THEN
                  UPDATE debt_agreement da
                     SET status = 1,
                         left_amount = 0
                   WHERE acckey = debtrec.acckey
                     AND agreementkey = debtrec.agreementkey
                     AND agreementnumb = debtrec.agreementnumb;

                  IF ABS (NVL (debtrec.left_amount, 0)) > min_gel
                  THEN
                     p_item_key :=
                        populate_item (rec.custkey,
                                       rec.acckey,
                                       NULL,
                                       NULL,
                                       oper_debt_activation,
                                       p_oper_key,
                                       NULL,
                                       rec.new_readdate,
                                       NULL,
                                       0,
                                       0,
                                       debtrec.left_amount,
                                       0,
                                       NULL,
                                       item_rec_common,
                                       NULL,
                                       NULL,
                                       NULL,
                                       p_details
                                      );
                  END IF;
               END IF;
            END LOOP;
         END IF;
      END LOOP;                                         -- end of the block loop

      -- Call accumulable subsidy
      accumulable_subsidy_2006.run_diffsubs_accumulation_2006 (p_block_key,
                                                               p_cycle_date,
                                                               p_oper_key
                                                              );

      -- Delete old for print table and details
      DELETE      for_print_detail
            WHERE forprintkey IN (SELECT for_printkey
                                    FROM for_print
                                   WHERE blockkey = p_block_key);

      DELETE      for_print
            WHERE blockkey = p_block_key;

      -- Save main bill printing information
      INSERT INTO for_print
                  (blockkey, readdate, senddate
                  )
           VALUES (p_block_key, p_cycle_date, SYSDATE
                  )
        RETURNING for_printkey
             INTO p_for_print_key;
             
---------------------------------------for base custcategory-----------22-apr-2010---------------------------------
   INSERT INTO for_print_detail
                  (forprintkey, custcatkey, blockkey, readdate, preparedcusts)
         SELECT p_for_print_key, r.custcatkey, p_block_key, p_cycle_date,
                r.custnum
           FROM (SELECT   categ.base_custcatkey custcatkey, COUNT (*) custnum
                     FROM ACCOUNT acc,
                          customer cust,custcateg categ,
                          (SELECT acckey
                             FROM route_store
                            WHERE blockkey = p_block_key
                              AND TO_NUMBER (TO_CHAR (new_readdate, 'J')) =
                                                                  p_cycle_date_j
                              AND status IN (0, 1)) rst
                    WHERE acc.custkey = cust.custkey
                    and cust.custcatkey=categ.custcatkey
                      AND acc.acckey = rst.acckey
                      AND acc.mainaccount = 1
                      AND cust.custcatkey NOT IN (4, 5, 6)
                  GROUP BY categ.base_custcatkey) r;
                  
-----------------------------------------------------------------------------------             

      -- calculate main account by category for further bill printing
   /*   INSERT INTO for_print_detail
                  (forprintkey, custcatkey, blockkey, readdate, preparedcusts)
         SELECT p_for_print_key, r.custcatkey, p_block_key, p_cycle_date,
                r.custnum
           FROM (SELECT   cust.custcatkey, COUNT (*) custnum
                     FROM ACCOUNT acc,
                          customer cust,
                          (SELECT acckey
                             FROM route_store
                            WHERE blockkey = p_block_key
                              AND TO_NUMBER (TO_CHAR (new_readdate, 'J')) =
                                                                  p_cycle_date_j
                              AND status IN (0, 1)) rst
                    WHERE acc.custkey = cust.custkey
                      AND acc.acckey = rst.acckey
                      AND acc.mainaccount = 1
                      AND cust.custcatkey NOT IN (4, 5, 6)
                 GROUP BY cust.custcatkey) r;
                 */
 
 -----------------------------------------------------------------------------------                  
                 

      IF SQL%NOTFOUND
      THEN
         DELETE      for_print
               WHERE for_printkey = p_for_print_key;
      END IF;

      -- Commit changes
      COMMIT;
   -- Exceptions block
   EXCEPTION
      -- rollback all changes and submit errors to the user form
      WHEN OTHERS
      THEN
         ROLLBACK;
         submit_error (SQLCODE,
                       SQLERRM || ' [Account ID] = ' || p_curr_account);
   END;                                     -- end of the 'calc_block' procedure

   /**
    * Process debt agreement for the account given in route store row.
    * This procedure was directly copyed from debt agreement processing part in
    * old BILL_MANAGER package ('send_to_print' procedure).
    * <p>
    *
    * Debt agreement is processed on cycle closing for each account. System
    * look ups for the active resheduled debt for the given account and than adds
    * monthly portions if active schedule is found.
    * <p>
    *
    * @param rs_record     record in BS.ROUTE_STORE table
    */
   PROCEDURE process_debt_agreement (rs_record IN route_store%ROWTYPE)
   IS
      debt_inserted   BOOLEAN := FALSE;
   BEGIN
      -- insert reschedule debt portions into item
      BEGIN
         INSERT INTO item
                     (schedkey, custkey, acckey, itemcatkey, billoperkey,
                      itemdate, amount, enterdate)
            SELECT rs_record.schedkey, rs_record.custkey, rs_record.acckey, 0,
                   oper_debt_reschedule, rs_record.new_readdate, d.amount,
                   rs_record.enterdate
              FROM (SELECT SUM (debt.payportion) AS amount
                      FROM debt_schedule debt, debt_agreement da
                     WHERE debt.acckey = rs_record.custkey
                       AND da.acckey = rs_record.custkey
                       AND da.acckey = debt.acckey
                       AND da.agreementkey = debt.agreementkey
                       AND TO_CHAR (rs_record.new_readdate, 'RRRRMM') >=
                                                TO_CHAR (debt.paydate, 'RRRRMM')
                       AND debt.status = 0
                       AND da.status = 0) d
             WHERE NVL (d.amount, 0) != 0;

         IF SQL%FOUND
         THEN
            debt_inserted := TRUE;
         END IF;
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      -- insert reschedule debt percents into item
      INSERT INTO item
                  (schedkey, custkey, acckey, itemcatkey, billoperkey, itemdate,
                   amount, enterdate)
         SELECT rs_record.schedkey, rs_record.custkey, rs_record.acckey, 0,
                oper_debt_reschedule_percent, rs_record.new_readdate, d.amount,
                rs_record.enterdate
           FROM (SELECT SUM (debt.PERCENT) AS amount
                   FROM debt_schedule debt
                  WHERE debt.acckey = rs_record.custkey
                    AND TO_CHAR (rs_record.new_readdate, 'RRRRMM') >=
                                                TO_CHAR (debt.paydate, 'RRRRMM')
                    AND debt.status = 0) d
          WHERE NVL (d.amount, 0) != 0;

      IF SQL%FOUND
      THEN
         debt_inserted := TRUE;
      END IF;

      IF debt_inserted
      THEN
         -- update debt agreement status
         UPDATE debt_agreement da
            SET da.left_amount =
                   (SELECT da.left_amount - SUM (ds.payportion)
                      FROM debt_schedule ds
                     WHERE ds.status = 0
                       AND ds.acckey = rs_record.custkey
                       AND TO_CHAR (rs_record.new_readdate, 'RRRRMM') >=
                                                  TO_CHAR (ds.paydate, 'RRRRMM')
                       AND da.agreementkey = ds.agreementkey)
          WHERE da.agreementkey IN (
                   SELECT DISTINCT ds1.agreementkey
                              FROM debt_schedule ds1
                             WHERE ds1.status = 0
                               AND ds1.acckey = rs_record.custkey
                               AND TO_CHAR (rs_record.new_readdate, 'RRRRMM') >=
                                                 TO_CHAR (ds1.paydate, 'RRRRMM'));

         UPDATE debt_schedule
            SET status = 1
          WHERE acckey = rs_record.custkey
            AND TO_CHAR (rs_record.new_readdate, 'RRRRMM') >=
                                                     TO_CHAR (paydate, 'RRRRMM')
            AND status = 0;
      END IF;
   END;                         -- end of the 'process_debt_agreement' procedure

   /**
    * New not-cycle reading processing procedure. This procedure is directly
    * called by the GUI form. After execution BS.ITEM table will be populated
    * with new rows. In most cases this is a single row, but in general case
    * there are more than one new rows.
    * <p>
    *
    * If execution fails, then error message will be supplied and all changes
    * rolled back.
    * <p>
    *
    * @param   p_acc_key       account key
    * @param   p_operation     code for the new not-cycle operation
    * @param   p_read_date     operation real date
    * @param   p_read_val      reading value for the account meter
    * @param   p_init_kwh      charged KWH, for some kind of readings
    * @param   p_init_gel      charged GEL, for some kind of readings
    * @param   p_operator      operator key
    * @param   p_sign_person   sign person key
    */
   PROCEDURE new_not_cycle_reading (
      p_acc_key       NUMBER,
      p_operation     NUMBER,
      p_read_date     DATE,
      p_read_val      NUMBER,
      p_init_kwh      NUMBER,
      p_init_gel      NUMBER,
      p_operator      NUMBER,
      p_sign_person   NUMBER
   )
   IS
      -- previous and previous real reading
      p_prev_read              item%ROWTYPE;
      p_prev_real_read         item%ROWTYPE;
      -- account information
      p_acc_cr_date            DATE;
      p_inst_cp                NUMBER;
      p_is_cutted              BOOLEAN;
      p_cust_key               NUMBER;
      p_cust_cat_key           NUMBER;
      p_mtt_type               NUMBER;
      p_mtt_digits             NUMBER;
      p_mtt_coeff              NUMBER;
      p_min_kwh                NUMBER;
      p_max_kwh                NUMBER;
      p_curr_balance           NUMBER;
      p_is_main_acc            BOOLEAN;
      p_is_closed              BOOLEAN;
      -- calc_kwh output parameters
      p_new_kwh                NUMBER;
      p_new_read_val           NUMBER;
      p_new_read_type          NUMBER;
      p_new_err_code           NUMBER;
      p_est_reading            NUMBER;
      p_force_est_correction   BOOLEAN;
      p_force_zero_amnt        BOOLEAN;
      p_valuable_item          BOOLEAN;
      -- item inserted
      p_item_key               NUMBER;
      -- other
      p_gel                    NUMBER;
      p_rec_cat                NUMBER;
      -- additional charge OUT parameters
      p_add_charge_item        NUMBER;
      p_add_charge_kwh         NUMBER;
      p_add_charge_gel         NUMBER;
      -- parameters for percent subsidy processing
      p_subs_kwh               NUMBER;
      p_subs_gel               NUMBER;
      p_subs_item              NUMBER;
      -- has step tariff?
      p_has_step_tariff        BOOLEAN;
      -- has empty history
      has_empty_history        BOOLEAN         := FALSE;
      -- dates interval
      d1                       DATE;
      d2                       DATE            := p_read_date;
      -- estimate processing parameters
      p_force_on_current       BOOLEAN;
      p_total_corrected        NUMBER;
      -- parent record
      p_parent                 tp_parent      := get_parent_account (p_acc_key);
      t1                       NUMBER;
      t2                       NUMBER;
      -- item details
      p_details                tp_item_details;
      -- send or not to parent?
      p_send_to_parent         BOOLEAN         := TRUE;
   BEGIN                           -- begin of 'new_not_cycle_reading' procedure
      -- Call clear warnings procedure
      clear_warnings;
      -- determine tariff structure
      p_has_step_tariff :=
                          has_step_tariff (p_acc_key, p_read_date, p_operation);
      -- Get previous reading information

      -- previous read
      p_prev_read := get_last_read (p_acc_key, NULL, FALSE);
      d1 := p_prev_read.itemdate;
      -- previous actual read
      p_prev_real_read := get_last_read (p_acc_key, NULL, TRUE);

      -- adjust previous reading information
      IF p_prev_read.itemkey IS NULL
      THEN
         p_prev_read.itemkey := 0;
         p_prev_read.reading := 0;
         p_prev_read.kwt := 0;
         p_prev_read.billoperkey := p_operation;
         p_prev_read.acckey := p_acc_key;
         p_prev_read.itemdate := infinity_past;
         has_empty_history := TRUE;
      END IF;

      -- Check estimates period

      -- When last reading is estimate and distinction between last reading
      -- and last actual reading was later than the controlable date, then do
      -- not proceed.
      -- The only exception is allowed when current operation type is one of
      -- 'meter install' or 'control reading'.
      IF     NVL (p_prev_read.itemkey, 0) != 0
         AND p_prev_real_read.itemkey IS NOT NULL
         AND p_prev_read.billoperkey = oper_estimate
         AND p_prev_real_read.itemdate < controlable_from
         AND p_operation NOT IN
                         (oper_control_reading, oper_meter_install, oper_audit)
      THEN
         raise_application_error
            (-20000,
                'Estimates period is too large. Please use voucher or act '
             || 'procedures before entering new reading. Or use ''metter install'' '
             || 'or ''constrol reading'' for entering it without previous period '
             || 'recalculation.'
            );
      END IF;

      -- Get account information
      get_acc_info (p_acc_key,
                    p_acc_cr_date,
                    p_inst_cp,
                    p_is_cutted,
                    p_cust_key,
                    p_cust_cat_key,
                    p_mtt_type,
                    p_mtt_digits,
                    p_mtt_coeff,
                    p_min_kwh,
                    p_max_kwh,
                    p_curr_balance,
                    p_is_main_acc,
                    p_is_closed
                   );

      -- adjust second date
      IF has_empty_history
      THEN
         d1 := p_acc_cr_date;
      END IF;

      -- Calcualte this reading

      -- special reading conditions
      IF    (    p_operation IN (oper_control_reading, oper_meter_install)
             AND (    ABS (NVL (p_new_kwh, 0)) > min_kwh
                  AND ABS (NVL (p_init_gel, 0)) > min_gel
                 )
            )
         OR p_operation = oper_audit
      THEN
         p_new_kwh := p_init_kwh;
         p_gel := p_init_gel;
         p_new_read_type := p_operation;
         p_force_est_correction := FALSE;
         p_valuable_item := FALSE;

         IF p_operation = oper_audit
         THEN
            p_new_kwh := 0;
            p_gel := 0;
         END IF;
      -- current cycle charges
      ELSIF p_operation IN
                         (oper_curr_charge_add, oper_curr_cycle_charge_voucher)
      THEN
         p_new_kwh := p_init_kwh;
         p_gel := p_init_gel;
         p_new_read_type := p_operation;
         p_new_read_val := p_read_val;

         -- @since 31-Aug-2006
         IF     NOT has_step_tariff (p_acc_key, p_read_date, 1)
            AND ABS (p_gel) < min_gel
            AND ABS (p_new_kwh) > min_kwh
         THEN
            raise_application_error (-20000,
                                        'Can not accept empty GEL amount '
                                     || 'for current charges.'
                                    );
         END IF;

         p_valuable_item := TRUE;
         p_force_est_correction := FALSE;
         p_send_to_parent := FALSE;
      -- common reading conditions
      ELSE
         calc_kwh (p_acc_key,
                   FALSE,
                   p_cust_cat_key,
                   p_inst_cp,
                   p_acc_cr_date,
                   p_is_cutted,
                   p_min_kwh,
                   p_max_kwh,
                   p_mtt_type,
                   p_mtt_digits,
                   p_mtt_coeff,
                   TRUE,
                   TRUE,
                   p_read_val,
                   p_operation,
                   p_read_date,
                   p_init_kwh,
                   p_init_gel,
                   p_prev_read,
                   p_prev_real_read,
                   p_new_kwh,
                   p_new_read_val,
                   p_new_read_type,
                   p_new_err_code,
                   p_force_zero_amnt,
                   p_est_reading,
                   p_force_est_correction
                  );
         p_new_kwh := ROUND (p_new_kwh);
         p_valuable_item := TRUE;

         -- when tariff is flat, then derive GEL charge
         IF NOT p_has_step_tariff
         THEN
            p_details := tp_item_details ();
            p_gel :=
               calc_gel (p_acc_key,
                         p_operation,
                         d1,
                         d2,
                         p_new_kwh,
                         FALSE,
                         p_details
                        );
         -- otherwise GEL charge is zero
         ELSE
            p_gel := 0;
         END IF;                             -- end of the step-tariff condition
      END IF;                                     -- end of adjustment condition

      -- Define record category
      IF p_valuable_item
      THEN
         p_rec_cat := item_rec_valuable;
      ELSE
         p_rec_cat := item_rec_common;
      END IF;

      -- Add reading to the ITEM table

      -- check whether estimates need to be corrected
      IF p_force_est_correction AND p_valuable_item
      THEN
         -- insert current reading
         p_item_key :=
            populate_item (p_cust_key,
                           p_acc_key,
                           NULL,
                           NULL,
                           p_new_read_type,
                           p_operator,
                           p_sign_person,
                           p_read_date,
                           NULL,
                           p_new_read_val,
                           0,
                           0,
                           0,
                           NULL,
                           item_rec_common,
                           NULL,
                           NULL,
                           NULL
                          );
         -- process estimates corrections
         process_estimate_correction (p_cust_key,
                                      p_acc_key,
                                      NULL,
                                      p_mtt_digits,
                                      p_mtt_coeff,
                                      p_new_read_val,
                                      p_new_read_type,
                                      p_read_date,
                                      p_prev_real_read,
                                      p_prev_read,
                                      p_operator,
                                      p_force_on_current,
                                      p_total_corrected
                                     );

         -- insert current charge
         IF ABS (NVL (p_new_kwh, 0)) > min_kwh OR p_force_on_current
         THEN
            -- adjust charges
            IF p_force_on_current
            THEN
               -- adjust current KWH charge
               p_new_kwh := p_new_kwh + NVL (p_total_corrected, 0);

               -- adjust current GEL charge
               IF NOT p_has_step_tariff
               THEN
                  p_details := tp_item_details ();
                  p_gel :=
                     calc_gel (p_acc_key,
                               p_new_read_type,
                               d1,
                               d2,
                               p_new_kwh,
                               FALSE,
                               p_details
                              );
               END IF;
            END IF;                                 -- end of charges adjustment

            IF p_force_on_current
            THEN
               UPDATE item
                  SET kwt = p_new_kwh,
                      amount = p_gel
                WHERE itemkey = p_item_key;

               add_item_details (p_item_key, p_details);
               -- balances may be not correct!
               bill_manager_2006.update_balances_history (p_cust_key,
                                                          p_item_key);
            ELSE
               p_item_key :=
                  populate_item (p_cust_key,
                                 p_acc_key,
                                 NULL,
                                 NULL,
                                 oper_curr_cycle_charge,
                                 p_operator,
                                 p_sign_person,
                                 p_read_date,
                                 NULL,
                                 0,
                                 p_new_kwh,
                                 p_gel,
                                 0,
                                 NULL,
                                 p_rec_cat,
                                 NULL,
                                 NULL,
                                 NULL,
                                 p_details
                                );
            END IF;
         END IF;
      -- plain reading
      ELSE
         -- insert current charge
         p_item_key :=
            populate_item (p_cust_key,
                           p_acc_key,
                           NULL,
                           NULL,
                           p_new_read_type,
                           p_operator,
                           p_sign_person,
                           p_read_date,
                           NULL,
                           p_read_val,
                           p_new_kwh,
                           p_gel,
                           0,
                           NULL,
                           p_rec_cat,
                           NULL,
                           NULL,
                           NULL,
                           p_details
                          );
      END IF;                                      -- end of estimates condition

      -- Check KWH charge
      IF ABS (NVL (p_new_kwh, 0)) > min_kwh
      THEN
         -- Process percent additional charges
         IF get_oper_cat (p_new_read_type) != oper_cat_add_charge
         THEN
            process_percent_add_charge (p_cust_key,
                                        p_acc_key,
                                        p_new_kwh,
                                        p_gel,
                                        NULL,
                                        p_read_date,
                                        p_item_key,
                                        p_operator,
                                        p_add_charge_item,
                                        p_add_charge_kwh,
                                        p_add_charge_gel
                                       );
         END IF;

         -- Process percent subsidies

         -- When additional charge has valuable kWh charging, than process
         -- subsidy as derived from total of the initial and additional
         -- charges. 'From' is the additional charge appropriate item.
         IF ABS (NVL (p_add_charge_kwh, 0)) > min_kwh
         THEN
            process_percent_subsidy (p_cust_key,
                                     p_acc_key,
                                     p_new_kwh + NVL (p_add_charge_kwh, 0),
                                     p_gel + NVL (p_add_charge_gel, 0),
                                     NULL,
                                     p_read_date,
                                     p_item_key,
                                     p_operator,
                                     p_subs_item,
                                     p_subs_kwh,
                                     p_subs_gel
                                    );
         -- When additional charge was not processed, than use initial kWh
         -- charging as subsidy base.
         ELSE
            process_percent_subsidy (p_cust_key,
                                     p_acc_key,
                                     p_new_kwh,
                                     p_gel,
                                     NULL,
                                     p_read_date,
                                     p_item_key,
                                     p_operator,
                                     p_subs_item,
                                     p_subs_kwh,
                                     p_subs_gel
                                    );
         END IF;                  -- end of additional charge checking condition

         -- Process parent charge
         IF ABS (NVL (p_new_kwh, 0)) > min_kwh AND p_send_to_parent
         THEN
            process_parent (p_parent,
                            p_item_key,
                            p_acc_key,
                            p_new_kwh,
                            NULL,
                            d1,
                            d2,
                            p_operator
                           );

            -- parent charge for the additional percent charge
            IF ABS (NVL (p_add_charge_kwh, 0)) > 0.01
            THEN
               process_parent (p_parent,
                               p_add_charge_item,
                               p_acc_key,
                               p_add_charge_kwh,
                               NULL,
                               d1,
                               d2,
                               p_operator
                              );
            END IF;
         END IF;                              -- end of process parent condition
      END IF;

      -- Commit changes
      COMMIT;
   -- Exceptions block
   EXCEPTION
      -- rollback all changes and submit errors to the user form
      WHEN OTHERS
      THEN
         ROLLBACK;
         submit_error (SQLCODE, SQLERRM);
   END;                          -- end of the 'new_not_cycle_reading' procedure

   /**
    * Calculate single cycle reading. The main information is submited using
    * record from the BS.ROUTE_STORE table.
    * <p>
    *
    * When parameter <tt>p_for_send_to_print</tt> is <tt>true</tt>, then
    * new records in BS.ITEM appers, account is then closing.
    * <p>
    *
    * @param  p_route_store_rec     record from the BS.ROUTE_STORE table
    * @param  p_for_send_to_print   close or not account? when <tt>true</tt>,
    *                               then account will be closed
    * @param  p_oper_key            operator key
    * @param  p_insp_key            inspector key
    */
   PROCEDURE new_cycle_reading (
      p_route_store_rec     route_store%ROWTYPE,
      p_for_send_to_print   BOOLEAN,
      p_oper_key            NUMBER,
      p_insp_key            NUMBER
   )
   IS
      -- account information details
      p_acc_cr_date         DATE;
      p_inst_cp             NUMBER;
      p_is_cutted           BOOLEAN;
      p_cust_key            NUMBER;
      p_cust_cat_key        NUMBER;
      p_mtt_type            NUMBER;
      p_mtt_digits          NUMBER;
      p_mtt_coeff           NUMBER;
      p_min_kwh             NUMBER;
      p_max_kwh             NUMBER;
      p_balance             NUMBER;
      p_is_main_acc         BOOLEAN;
      p_is_closed           BOOLEAN;
      -- metter and seal statuses
      p_mtt_curr_stat       BOOLEAN;
      p_mtt_new_stat        BOOLEAN;
      p_seal_curr_stat      BOOLEAN;
      p_seal_new_stat       BOOLEAN;
      -- previous reading values
      p_prev_read           item%ROWTYPE;
      p_prev_real_read      item%ROWTYPE;
      -- calc_kwh output
      p_final_kwh           NUMBER;
      p_final_read_val      NUMBER;
      p_final_read_type     NUMBER;
      p_final_err_code      NUMBER;
      p_est_reading         NUMBER;
      p_force_est_correct   BOOLEAN;
      p_force_zero_amount   BOOLEAN;
      p_valuable_item       BOOLEAN;
      -- dates
      d1                    DATE;
      d2                    DATE;
      -- full kWh charge
      p_full_kwh            NUMBER;
   BEGIN                           -- begin of the 'new_cycle_reading' procedure
      -- Initialize account information

      -- getting account information
      get_acc_info (p_route_store_rec.acckey,
                    p_acc_cr_date,
                    p_inst_cp,
                    p_is_cutted,
                    p_cust_key,
                    p_cust_cat_key,
                    p_mtt_type,
                    p_mtt_digits,
                    p_mtt_coeff,
                    p_min_kwh,
                    p_max_kwh,
                    p_balance,
                    p_is_main_acc,
                    p_is_closed
                   );
      -- adjust account info
      p_max_kwh := p_route_store_rec.max_charge;
      p_min_kwh := p_route_store_rec.min_charge;
      -- metter and seal statuses
      p_mtt_curr_stat := NVL (p_route_store_rec.cur_mtstat, 0) = 0;
      p_mtt_new_stat := NVL (p_route_store_rec.new_mtstat, 0) = 0;
      p_seal_curr_stat := NVL (p_route_store_rec.cur_sealstat, 0) = 0;
      p_seal_new_stat := NVL (p_route_store_rec.new_sealstat, 0) = 0;
      -- Get readings information

      -- initialize readings
      p_prev_read := get_last_read (p_route_store_rec.acckey, NULL, FALSE);
      p_prev_real_read := get_last_read (p_route_store_rec.acckey, NULL, TRUE);

      -- adjust previous reading information
      IF p_prev_read.itemkey IS NULL
      THEN
         p_prev_read.itemkey := 0;
         p_prev_read.reading := 0;
         p_prev_read.kwt := 0;
         p_prev_read.acckey := p_route_store_rec.acckey;
         p_prev_read.itemdate := p_acc_cr_date;
      END IF;

      -- Calcualate reading

      -- calculate KWH
      calc_kwh (p_route_store_rec.acckey,
                TRUE,
                p_cust_cat_key,
                p_inst_cp,
                p_acc_cr_date,
                p_is_cutted,
                p_min_kwh,
                p_max_kwh,
                p_mtt_type,
                p_mtt_digits,
                p_mtt_coeff,
                p_mtt_curr_stat AND p_mtt_new_stat,
                p_seal_curr_stat AND p_seal_new_stat,
                p_route_store_rec.new_reading,
                p_route_store_rec.new_rdtype,
                p_route_store_rec.new_readdate,
                0,
                0,
                p_prev_read,
                p_prev_real_read,
                p_final_kwh,
                p_final_read_val,
                p_final_read_type,
                p_final_err_code,
                p_force_zero_amount,
                p_est_reading,
                p_force_est_correct
               );

      -- Process in route store

      -- populate route store with estimate correction
      IF p_force_est_correct
      THEN
         -- derive full kWh charge
         p_full_kwh :=
            derive_kwh (p_route_store_rec.new_reading,
                        p_prev_real_read.reading,
                        p_prev_real_read.reading,
                        p_mtt_coeff,
                        p_mtt_digits
                       );
         -- populate route store
         populate_route_store (p_final_err_code,
                               p_final_kwh,
                               p_final_read_type,
                               p_route_store_rec,
                               p_final_read_val,
                               p_for_send_to_print,
                               p_full_kwh
                              );
      -- populate route store without estimate correction
      ELSE
         -- populate route store
         populate_route_store (p_final_err_code,
                               p_final_kwh,
                               p_final_read_type,
                               p_route_store_rec,
                               p_final_read_val,
                               p_for_send_to_print
                              );
      END IF;

      -- Close accout

      -- when sending to print, close account
      IF p_for_send_to_print
      THEN
         -- close account
         close_account (p_cust_key,
                        p_route_store_rec.acckey,
                        p_is_main_acc,
                        p_acc_cr_date,
                        p_mtt_digits,
                        p_mtt_coeff,
                        p_final_kwh,
                        p_final_read_val,
                        p_final_read_type,
                        p_route_store_rec.schedkey,
                        p_route_store_rec.new_readdate,
                        p_prev_read.itemdate,
                        p_prev_real_read,
                        p_prev_read,
                        p_force_est_correct,
                        p_oper_key,
                        p_insp_key
                       );
      END IF;                                  -- end of send to print condition
   END;                              -- end of the 'new_cycle_reading' procedure

   /**
    * Close account. This is called by the new_cycle_reading procedure when
    * closing cycle.
    *
    * @param   p_customer          customer key
    * @param   p_account           account key
    * @param   p_is_main_acc       is main account?
    * @param   p_acc_creation      account creation date
    * @param   p_mt_digits         meter digits
    * @param   p_mt_coeff          meter coefficient
    * @param   p_new_kwh_init      initial value for kWh charging
    * @param   p_new_read_val      new reading value
    * @param   p_new_read_type     new reading type
    * @param   p_schedule          schedule key
    * @param   p_new_read_date     new reading date
    * @param   p_prev_read_date    previous reading datye
    * @param   p_prev_real_read    previous real reading
    * @param   p_prev_read         previous reading
    * @param   p_force_est_correct force or not estimates correction?
    * @param   p_operator          operator key
    * @param   p_inspector         inspector key
    */
   PROCEDURE close_account (
      p_customer            NUMBER,
      p_account             NUMBER,
      p_is_main_acc         BOOLEAN,
      p_acc_creation        DATE,
      p_mt_digits           NUMBER,
      p_mt_coeff            NUMBER,
      p_new_kwh_init        NUMBER,
      p_new_read_val        NUMBER,
      p_new_read_type       NUMBER,
      p_schedule            NUMBER,
      p_new_read_date       DATE,
      p_prev_read_date      DATE,
      p_prev_real_read      item%ROWTYPE,
      p_prev_read           item%ROWTYPE,
      p_force_est_correct   BOOLEAN,
      p_operator            NUMBER,
      p_inspector           NUMBER
   )
   IS
      p_new_kwh                     NUMBER          := p_new_kwh_init;
      -- additional charge parameters
      p_add_kwh_item                NUMBER;
      p_add_percent_item            NUMBER;
      p_add_subs_item               NUMBER;
      p_subs_kwh_item               NUMBER;
      p_subs_kwh_kwh                NUMBER;
      p_subs_kwh_gel                NUMBER;
      p_add_percent_kwh             NUMBER;
      p_add_kwh_kwh                 NUMBER;
      p_subs_percent_kwh            NUMBER;
      p_add_percent_gel             NUMBER;
      p_add_kwh_gel                 NUMBER;
      p_subs_percent_gel            NUMBER;
      -- temporary parameters
      p_temp_kwh                    NUMBER;
      p_temp_gel                    NUMBER;
      -- temporary item keys
      p_read_item_key               NUMBER;
      -- has step tariff?
      p_has_step_tariff             BOOLEAN;
      -- gel charge appropriate to 'p_new_kwh' charge
      p_new_gel                     NUMBER          := 0;
      -- flags
      p_has_gel_charge              BOOLEAN         := FALSE;
      p_has_empty_gel_charge        BOOLEAN         := FALSE;
      p_cycle                       NUMBER;
      p_total_cycle_kwh             NUMBER          := 0;
      p_total_cycle_gel             NUMBER          := 0;
      p_total_cycle_subs_gel        NUMBER          := 0;
      d1                            DATE;
      d2                            DATE;
      p_temp_item                   NUMBER;
      -- estimate processing parameters
      p_force_on_current            BOOLEAN;
      p_total_corrected             NUMBER;
      -- percent subsidy
      p_total_psubs_kwh             NUMBER          := 0;
      p_total_psubs_gel             NUMBER          := 0;
      -- parent record
      p_parent                      tp_parent := get_parent_account (p_account);
      -- previous cycle last item
      p_prev_cycle_last_item        NUMBER;
      p_prev_cycle_last_enterdate   DATE;
      -- item details
      p_details                     tp_item_details;
   BEGIN                               -- begin of the 'close_account' procedure
      -- Get cycle
      p_cycle := get_cycle (p_schedule);
      -- Define interval dates

      -- @since 11/08/2006: here was code which was moved to the
      -- 'get_cycle_start_point' function: now it looks more fine and may be used
      -- also in another places of this package
      d2 := TRUNC (p_new_read_date);
      d1 :=
         get_cycle_start_point (d2,
                                p_account,
                                p_schedule,
                                p_acc_creation,
                                p_prev_cycle_last_enterdate,
                                p_prev_cycle_last_item
                               );
      -- has step during the cycle?
      p_has_step_tariff := has_step_tariff (p_account, d1, d2, p_new_read_type);

      -- derive GEL charge value for the kwh charge
      IF NOT p_has_step_tariff
      THEN
         p_details := tp_item_details ();
         p_new_gel :=
            calc_gel (p_account,
                      p_new_read_type,
                      p_prev_read_date,
                      p_new_read_date,
                      p_new_kwh,
                      FALSE,
                      p_details
                     );
      END IF;

      -- Process estimates correction

      -- correct estimates
      IF p_force_est_correct
      THEN
         -- insert current reading
         p_read_item_key :=
            populate_item (p_customer,
                           p_account,
                           NULL,
                           p_schedule,
                           p_new_read_type,
                           p_operator,
                           p_inspector,
                           p_new_read_date,
                           NULL,
                           p_new_read_val,
                           0,
                           0,
                           0,
                           NULL,
                           item_rec_valuable,
                           NULL,
                           NULL,
                           NULL
                          );
         -- process estimates corrections
         process_estimate_correction (p_customer,
                                      p_account,
                                      p_schedule,
                                      p_mt_digits,
                                      p_mt_coeff,
                                      p_new_read_val,
                                      p_new_read_type,
                                      p_new_read_date,
                                      p_prev_real_read,
                                      p_prev_read,
                                      p_operator,
                                      p_force_on_current,
                                      p_total_corrected
                                     );

         -- adjust charges
         IF p_force_on_current
         THEN
            -- adjust current KWH charge
            p_new_kwh := p_new_kwh + NVL (p_total_corrected, 0);

            -- adjust current GEL charge
            IF NOT p_has_step_tariff
            THEN
               p_details := tp_item_details ();
               p_new_gel :=
                  calc_gel (p_account,
                            p_new_read_type,
                            p_prev_read_date,
                            p_new_read_date,
                            p_new_kwh,
                            FALSE,
                            p_details
                           );
            END IF;
         END IF;

         IF p_force_on_current
         THEN
            -- update current charge
            UPDATE item
               SET kwt = p_new_kwh,
                   amount = p_new_gel
             WHERE itemkey = p_read_item_key;

            -- add item details
            add_item_details (p_read_item_key, p_details);
            -- balance may be not correct!
            bill_manager_2006.update_balances_history (p_customer,
                                                       p_read_item_key
                                                      );
         ELSE
            -- insert current charge
            p_read_item_key :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              oper_curr_cycle_charge,
                              p_operator,
                              p_inspector,
                              p_new_read_date,
                              NULL,
                              0,
                              p_new_kwh,
                              p_new_gel,
                              0,
                              NULL,
                              item_rec_valuable,
                              NULL,
                              NULL,
                              NULL,
                              p_details
                             );
         END IF;
      -- without estimate correction
      ELSE
         -- insert current reading and kWh
         p_read_item_key :=
            populate_item (p_customer,
                           p_account,
                           NULL,
                           p_schedule,
                           p_new_read_type,
                           p_operator,
                           p_inspector,
                           p_new_read_date,
                           NULL,
                           p_new_read_val,
                           p_new_kwh,
                           p_new_gel,
                           0,
                           NULL,
                           item_rec_valuable,
                           NULL,
                           NULL,
                           NULL,
                           p_details
                          );
      END IF;                           -- end of estimates correction condition

      -- Additional charges and subsidies

      -- Percent additional charges

      -- process percent additional charge
      process_percent_add_charge (p_customer,
                                  p_account,
                                  p_new_kwh,
                                  p_new_gel,
                                  p_schedule,
                                  p_new_read_date,
                                  p_read_item_key,
                                  p_operator,
                                  p_add_percent_item,
                                  p_add_percent_kwh,
                                  p_add_percent_gel
                                 );
      -- Additional KWH charges

      -- process KWH additional charges
      process_kwh_add_charge (p_customer,
                              p_account,
                              p_schedule,
                              d1,
                              d2,
                              p_operator,
                              NOT p_has_step_tariff,
                              p_add_kwh_item,
                              p_add_kwh_kwh,
                              p_add_kwh_gel
                             );
      -- Make additional charges summary

      -- summarize additional charges and subsidies
      p_temp_kwh :=
                  p_new_kwh + NVL (p_add_percent_kwh, 0)
                  + NVL (p_add_kwh_kwh, 0);
      p_temp_gel :=
                  p_new_gel + NVL (p_add_percent_gel, 0)
                  + NVL (p_add_kwh_gel, 0);
      -- Percent subsidies

      -- process percent subsidies
      process_percent_subsidy (p_customer,
                               p_account,
                               p_temp_kwh,
                               p_temp_gel,
                               p_schedule,
                               p_new_read_date,
                               p_read_item_key,
                               p_operator,
                               p_add_subs_item,
                               p_subs_percent_kwh,
                               p_subs_percent_gel
                              );

      -- Process parent

      -- main charge and additional percent charge for parent
      IF ABS (NVL (p_new_kwh, 0)) > min_kwh
      THEN
         -- main charge
         process_parent (p_parent,
                         p_read_item_key,
                         p_account,
                         p_new_kwh,
                         p_schedule,
                         p_prev_read_date,
                         p_new_read_date,
                         p_operator
                        );

         -- additional percent charge
         IF ABS (NVL (p_add_percent_kwh, 0)) > min_kwh
         THEN
            process_parent (p_parent,
                            p_add_percent_item,
                            p_account,
                            p_add_percent_kwh,
                            p_schedule,
                            p_prev_read_date,
                            p_new_read_date,
                            p_operator
                           );
         END IF;
      END IF;

      -- additional KWH charge for parent
      IF ABS (NVL (p_add_kwh_kwh, 0)) > min_kwh
      THEN
         process_parent (p_parent,
                         p_add_kwh_item,
                         p_account,
                         p_add_kwh_kwh,
                         p_schedule,
                         p_prev_read_date,
                         p_new_read_date,
                         p_operator
                        );
      END IF;

      -- Update "none" cycles in ITEM_RELATION table
      IF p_prev_cycle_last_item IS NOT NULL
      THEN
         -- update this account directly
         UPDATE item_relation
            SET ir_cycle = p_cycle
          WHERE ir_account = p_account AND ir_item > p_prev_cycle_last_item;
      ELSE
         -- update all records for this account
         UPDATE item_relation
            SET ir_cycle = p_cycle
          WHERE ir_account = p_account;

         p_prev_cycle_last_item := 0;
      END IF;

      -- update this account as child account
      UPDATE item_relation
         SET ir_child_cycle = p_cycle
       WHERE ir_child = p_account AND ir_child_cycle = cycle_none;

      -- Summarize not-cycle kWh charges for this account and all valuable
      -- charges for other accounts

      -- loop over all valuable items
      FOR rec IN (SELECT i.*, bop.opertpkey
                    FROM item i, item_relation ir, billoperation bop
                   WHERE i.itemkey = ir.ir_item
                     AND ir.ir_item > p_prev_cycle_last_item
                     AND ir.ir_account = p_account
                     AND (   ir.ir_type = item_rec_valuable
                          OR ir.ir_type = item_rec_percent_charge
                         )
                     AND i.billoperkey = bop.billoperkey
                     AND
                         -- @since 11/07/2006: condition on ENTERDATE to improve performance
                         i.enterdate > p_prev_cycle_last_enterdate - 30)
      LOOP
         -- @since 20/07/2006
         -- This is a very important condition!!!!!! Do not delete it!!!!!
         IF rec.itemdate >= d1
         THEN
            -- when we have no amount charge
            IF     ABS (NVL (rec.amount, 0)) < min_gel
               AND ABS (NVL (rec.kwt, 0)) > min_kwh
            THEN
               -- has kWh charge without amount charging
               p_has_empty_gel_charge := TRUE;

               -- percent subsidy charges
               IF rec.opertpkey = oper_cat_subsidy
               THEN
                  p_total_psubs_kwh := p_total_psubs_kwh + rec.kwt;
               -- other charges
               ELSE
                  p_total_cycle_kwh := p_total_cycle_kwh + rec.kwt;
               END IF;
            -- when we have GEL charge
            ELSIF ABS (NVL (rec.amount, 0)) > min_gel
            --NVL(rec.AMOUNT, 0) > MIN_GEL -- only positive GEL charges!!!
            THEN
               -- has GEL charge
               p_has_gel_charge := TRUE;

               -- subsidy charges (percent subsidy only!)
               IF rec.opertpkey = oper_cat_subsidy
               THEN
                  p_total_cycle_subs_gel := p_total_cycle_subs_gel + rec.amount;
                  p_total_psubs_kwh := p_total_psubs_kwh + rec.kwt;
               -- other charges
               ELSE
                  p_total_cycle_gel := p_total_cycle_gel + rec.amount;
                  p_total_cycle_kwh := p_total_cycle_kwh + rec.kwt;
               END IF;
            ELSE
               -- ignore this case: empty GEL and KWH charges
               NULL;
            END IF;                          -- end of charge checking condition
         END IF;
      END LOOP;                                      -- end of kWh summarization

      -- Make additional records on the cycle closing

      -- @since 21/02/2007: this IF block, which additionaly checks
      --                    step-tariff existence
      IF p_has_empty_gel_charge
      THEN
         IF NOT has_step_tariff (p_account, d1, d2)
         THEN
            p_has_empty_gel_charge := FALSE;
         END IF;
      END IF;

      -- #A. there are no empty GEL charges, then try to process KWH subsidies
      IF NOT p_has_empty_gel_charge
      THEN
         -- KWH subsidies here
         IF NOT p_has_step_tariff
         THEN
            process_kwh_subsidy (p_customer,
                                 p_account,
                                 p_schedule,
                                 d1,
                                 d2,
                                 p_operator,
                                 NOT p_has_step_tariff,
                                 p_subs_kwh_item,
                                 p_subs_kwh_kwh,
                                 p_subs_kwh_gel
                                );
         END IF;
      -- #2. when not have any GEL charge, then make summary
      ELSIF NOT p_has_gel_charge
      THEN
         -- get cycle charge
         p_details := tp_item_details ();
         p_total_cycle_gel :=
            calc_gel (p_account, 1, d1, d2, p_total_cycle_kwh, FALSE,
                      p_details);

         -- when additional charges exists
         IF ABS (NVL (p_total_cycle_kwh, 0) - NVL (p_new_kwh, 0)) > min_kwh
         THEN
            -- make summary only when GEL charge exists
            IF ABS (NVL (p_total_cycle_gel, 0)) > min_gel
            THEN
               -- make summary record (using MAIN account tariff!)
               p_temp_item :=
                  populate_item (p_customer,
                                 p_account,
                                 NULL,
                                 p_schedule,
                                 oper_summary,
                                 p_operator,
                                 p_inspector,
                                 p_new_read_date,
                                 NULL,
                                 0,
                                 p_total_cycle_kwh,
                                 p_total_cycle_gel,
                                 0,
                                 NULL,
                                 item_rec_common,
                                 NULL,
                                 NULL,
                                 NULL,
                                 p_details
                                );
            END IF;
         -- when no additional record required
         ELSE
            UPDATE item
               SET amount = p_total_cycle_gel
             WHERE itemkey = p_read_item_key;

            add_item_details (p_read_item_key, p_details);
            -- balances may be not correct
            bill_manager_2006.update_balances_history (p_customer,
                                                       p_read_item_key
                                                      );
         END IF;                                 -- end of make record condition

         -- Process percent subsidy
         IF ABS (NVL (p_total_cycle_kwh, 0)) > min_kwh
         THEN
            p_total_psubs_gel :=
                      p_total_cycle_gel * p_total_psubs_kwh / p_total_cycle_kwh;

            IF ABS (NVL (p_total_psubs_gel, 0)) > min_gel
            THEN
               p_temp_item :=
                  populate_item (p_customer,
                                 p_account,
                                 NULL,
                                 p_schedule,
                                 oper_psubs_summary,
                                 p_operator,
                                 p_inspector,
                                 p_new_read_date,
                                 NULL,
                                 0,
                                 p_total_psubs_kwh,
                                 p_total_psubs_gel,
                                 0,
                                 NULL,
                                 item_rec_common,
                                 NULL,
                                 NULL,
                                 NULL
                                );
            END IF;
         END IF;                                  -- end of % subsidy processing
      -- #3. has as gel-charged kWh-s, also kWh-s without gel-charge
      ELSE
         -- discharge subsidy GEL
         IF ABS (NVL (p_total_cycle_subs_gel, 0)) > min_gel
         THEN
            -- discharge GEL
            p_temp_item :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              oper_subsidy_gel_discharge,
                              p_operator,
                              NULL,
                              p_new_read_date,
                              NULL,
                              0,
                              0,
                              -p_total_cycle_subs_gel,
                              0,
                              NULL,
                              item_rec_common,
                              NULL,
                              NULL,
                              NULL
                             );
         END IF;

         -- discharge valuable GEL
         IF ABS (NVL (p_total_cycle_gel, 0)) > min_gel
         THEN
            -- discharge GEL
            p_temp_item :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              oper_curr_cycle_gel_discharge,
                              p_operator,
                              NULL,
                              p_new_read_date,
                              NULL,
                              0,
                              0,
                              -p_total_cycle_gel,
                              0,
                              NULL,
                              item_rec_common,
                              NULL,
                              NULL,
                              NULL
                             );
         END IF;

         -- get cycle charge
         p_details := tp_item_details ();
         p_total_cycle_gel :=
            calc_gel (p_account, 1, d1, d2, p_total_cycle_kwh, FALSE, p_details);
         -- make summary record
         p_temp_item :=
            populate_item (p_customer,
                           p_account,
                           NULL,
                           p_schedule,
                           oper_summary,
                           p_operator,
                           p_inspector,
                           p_new_read_date,
                           NULL,
                           0,
                           p_total_cycle_kwh,
                           p_total_cycle_gel,
                           0,
                           NULL,
                           item_rec_common,
                           NULL,
                           NULL,
                           NULL,
                           p_details
                          );

         -- Process percent subsidy
         IF ABS (NVL (p_total_cycle_kwh, 0)) > min_kwh
         THEN
            p_total_psubs_gel :=
                      p_total_cycle_gel * p_total_psubs_kwh / p_total_cycle_kwh;

            IF ABS (NVL (p_total_psubs_gel, 0)) > min_gel
            THEN
               p_temp_item :=
                  populate_item (p_customer,
                                 p_account,
                                 NULL,
                                 p_schedule,
                                 oper_psubs_summary,
                                 p_operator,
                                 p_inspector,
                                 p_new_read_date,
                                 NULL,
                                 0,
                                 p_total_psubs_kwh,
                                 p_total_psubs_gel,
                                 0,
                                 NULL,
                                 item_rec_common,
                                 NULL,
                                 NULL,
                                 NULL
                                );
            END IF;
         END IF;                                  -- end of % subsidy processing
      END IF;                                        -- end of charge conditions

      -- process GEL subsidies/additional charges
      process_gel_charges (p_customer,
                           p_account,
                           p_schedule,
                           p_new_read_date,
                           p_operator
                          );
   END;                                  -- end of the 'close_account' procedure

   /**
    * This procedure calculates KWH charge value for the given reading. It also
    * gives perception how to calculate GEL amount and how to manage possible
    * estimates.
    */
   PROCEDURE calc_kwh (
      p_account                         NUMBER,
      -- general parameters
      p_is_cycle_reading                BOOLEAN,
      p_cust_cat_key                    NUMBER,
      -- account information
      p_acc_inst_cp                     NUMBER,
      p_acc_cr_date                     DATE,
      p_is_cutted                       BOOLEAN,
      p_min_kwh                         NUMBER,
      p_max_kwh                         NUMBER,
      -- meter conditions
      p_mt_type                         NUMBER,
      p_mt_digits                       NUMBER,
      p_mt_coeff                        NUMBER,
      p_mt_stat                         BOOLEAN,
      p_seal_stat                       BOOLEAN,
      -- current reading information
      p_curr_read_val                   NUMBER,
      p_curr_read_type                  NUMBER,
      p_curr_read_date                  DATE,
      p_curr_kwh                        NUMBER,
      p_curr_amnt                       NUMBER,
      -- previous reading information
      p_prev_read                       item%ROWTYPE,
      p_prev_act_read                   item%ROWTYPE,
      -- procedure output
      p_new_kwh                IN OUT   NUMBER,
      p_new_read_val           IN OUT   NUMBER,
      p_new_read_type          IN OUT   NUMBER,
      p_new_err_code           IN OUT   NUMBER,
      p_force_zero_amnt        IN OUT   BOOLEAN,
      -- estimate reading correction options
      p_est_reading            IN OUT   NUMBER,
      p_force_est_correction   IN OUT   BOOLEAN
   )
   IS
   BEGIN                                        -- begin of 'calc_kwh' procedure
      -- Check dates

      -- check essential dates not to be empty
      IF    p_curr_read_date IS NULL
         OR p_prev_read.itemdate IS NULL
         OR p_acc_cr_date IS NULL
      THEN
         raise_application_error
            (-20000,
                'One of current reading, previous reading or account creation '
             || 'dates is empty.'
            );
      -- current operation date is earlier then the previous reading
      -- date or account creation date
      ELSIF    TRUNC (p_curr_read_date) <
                               TRUNC (NVL (p_prev_read.itemdate, infinity_past))
            OR TRUNC (p_curr_read_date) < TRUNC (p_acc_cr_date)
      THEN
         raise_application_error
            (-20000,
                'Current operation date is less then the previous operation or '
             || 'account creation date.'
            );
      -- current operation date is earlier then the previous actual reading date
      -- this is checked when actual reading date is not NULL
      ELSIF     p_prev_act_read.itemdate IS NOT NULL
            AND TRUNC (p_curr_read_date) < TRUNC (p_prev_act_read.itemdate)
      THEN
         raise_application_error
            (-20000,
                'Current operation date is less then the previous actual reading '
             || 'date.'
            );
      END IF;                                 -- end of dates checking condition

      -- Initialize output parameters
      p_new_kwh := NVL (p_curr_kwh, 0);
      p_new_read_val := NVL (p_curr_read_val, 0);
      p_new_read_type := NVL (p_curr_read_type, 0);
      p_est_reading := 0;
      p_force_est_correction := FALSE;
      p_force_zero_amnt := FALSE;

      -- Calculate readings

      -- Cycle reading
      IF p_is_cycle_reading IS NOT NULL AND p_is_cycle_reading
      THEN
         -- call cycle reading processing procedure
         calc_cycle_reading (p_account,
                             p_cust_cat_key,
                             p_acc_inst_cp,
                             p_acc_cr_date,
                             p_is_cutted,
                             p_min_kwh,
                             p_max_kwh,
                             p_mt_type,
                             p_mt_digits,
                             p_mt_coeff,
                             p_mt_stat,
                             p_seal_stat,
                             p_curr_read_val,
                             p_curr_read_date,
                             p_prev_read,
                             p_prev_act_read,
                             p_new_kwh,
                             p_new_read_val,
                             p_new_read_type,
                             p_new_err_code,
                             p_force_zero_amnt,
                             p_est_reading,
                             p_force_est_correction
                            );
      -- Not cycle reading
      ELSE
         -- call not-cycle reading processing procedure
         calc_not_cycle_reading (p_cust_cat_key,
                                 p_acc_inst_cp,
                                 p_acc_cr_date,
                                 p_is_cutted,
                                 p_mt_digits,
                                 p_mt_coeff,
                                 p_curr_read_val,
                                 p_curr_read_type,
                                 p_curr_read_date,
                                 p_curr_kwh,
                                 p_curr_amnt,
                                 p_prev_read,
                                 p_prev_act_read,
                                 p_new_kwh,
                                 p_new_read_val,
                                 p_new_read_type,
                                 p_force_zero_amnt,
                                 p_est_reading,
                                 p_force_est_correction
                                );
      END IF;                       -- end of cycle/not-cycle condition checking
   END;                                       -- end of the 'calc_kwh' procedure

   /**
    * This procedure is used by 'calc_kwh' function to process cycle reading
    * information.
    */
   PROCEDURE calc_cycle_reading (
      p_account                         NUMBER,
      p_cust_cat_key                    NUMBER,
      -- account information
      p_acc_inst_cp                     NUMBER,
      p_acc_cr_date                     DATE,
      p_is_cutted                       BOOLEAN,
      p_min_kwh                         NUMBER,
      p_max_kwh                         NUMBER,
      -- meter conditions
      p_mt_type                         NUMBER,
      p_mt_digits                       NUMBER,
      p_mt_coeff                        NUMBER,
      p_mt_stat                         BOOLEAN,
      p_seal_stat                       BOOLEAN,
      -- current reading information
      p_curr_read_val                   NUMBER,
      p_curr_read_date                  DATE,
      -- previous reading information
      p_prev_read                       item%ROWTYPE,
      p_prev_act_read                   item%ROWTYPE,
      -- procedure IN OUTput
      p_new_kwh                IN OUT   NUMBER,
      p_new_read_val           IN OUT   NUMBER,
      p_new_read_type          IN OUT   NUMBER,
      p_new_err_code           IN OUT   NUMBER,
      p_force_zero_amnt        IN OUT   BOOLEAN,
      -- estimate reading correction options
      p_est_reading            IN OUT   NUMBER,
      p_force_est_correction   IN OUT   BOOLEAN
   )
   IS
      -- exists or not current reading value?
      has_current_reading           BOOLEAN := NOT NVL (p_curr_read_val, 0) = 0;
      p_prev_cycle_last_item        NUMBER;
      p_prev_cycle_last_itemdate    DATE;
      p_prev_cycle_last_enterdate   DATE;
   BEGIN
      -- Current reading exists
      IF has_current_reading
      THEN
         -- calculate cycle when reading exists
         calc_cycle_with_reading (p_cust_cat_key,
                                  p_acc_inst_cp,
                                  p_acc_cr_date,
                                  p_is_cutted,
                                  p_min_kwh,
                                  p_max_kwh,
                                  p_mt_type,
                                  p_mt_digits,
                                  p_mt_coeff,
                                  p_mt_stat,
                                  p_seal_stat,
                                  p_curr_read_val,
                                  p_curr_read_date,
                                  p_prev_read,
                                  p_prev_act_read,
                                  p_new_kwh,
                                  p_new_read_val,
                                  p_new_read_type,
                                  p_new_err_code,
                                  p_force_zero_amnt,
                                  p_est_reading,
                                  p_force_est_correction
                                 );
      -- Current reading does not exist
      ELSE
         -- calculate cycle when reading does not exist
         calc_cycle_without_reading (p_acc_inst_cp,
                                     p_acc_cr_date,
                                     p_is_cutted,
                                     p_min_kwh,
                                     p_max_kwh,
                                     p_mt_type,
                                     p_mt_digits,
                                     p_mt_coeff,
                                     p_mt_stat,
                                     p_seal_stat,
                                     p_curr_read_date,
                                     p_prev_read,
                                     p_prev_act_read,
                                     p_new_kwh,
                                     p_new_read_val,
                                     p_new_read_type,
                                     p_new_err_code,
                                     p_force_zero_amnt,
                                     p_est_reading,
                                     p_force_est_correction
                                    );
      END IF;                      -- end of reading exists/not exists condition

      -- @since 20/07/2006 [TODO:] revise this

      -- This checks whether previous cycle was so far

      -- get previous cycle information
      get_previous_cycle_info (p_account,
                               NULL,
                               p_prev_cycle_last_item,
                               p_prev_cycle_last_itemdate,
                               p_prev_cycle_last_enterdate
                              );

      -- check previous cycle relation
      IF ABS (p_prev_cycle_last_itemdate - p_curr_read_date) >
                                                  previous_cycle_max_distinction
      THEN
         IF p_prev_cycle_last_item = -1
         THEN
            IF ABS (p_acc_cr_date - p_curr_read_date) >
                                                 previous_cycle_max_distinction
            THEN
               p_new_err_code := err_23;
            END IF;
         ELSE
            p_new_err_code := err_23;
         END IF;
      END IF;
   END;                             -- end of the 'calc_cycle_reading' procedure

   /**
    * Calculates cycle reading when current reading exists.
    * This procedure is called by 'calc_cycle_reading' procedure when
    * current reading exists.
    */
   PROCEDURE calc_cycle_with_reading (
      p_cust_cat_key                    NUMBER,
      -- account information
      p_acc_inst_cp                     NUMBER,
      p_acc_cr_date                     DATE,
      p_is_cutted                       BOOLEAN,
      p_min_kwh                         NUMBER,
      p_max_kwh                         NUMBER,
      -- meter conditions
      p_mt_type                         NUMBER,
      p_mt_digits                       NUMBER,
      p_mt_coeff                        NUMBER,
      p_mt_stat                         BOOLEAN,
      p_seal_stat                       BOOLEAN,
      -- current reading information
      p_curr_read_val                   NUMBER,
      p_curr_read_date                  DATE,
      -- previous reading information
      p_prev_read                       item%ROWTYPE,
      p_prev_act_read                   item%ROWTYPE,
      -- procedure IN OUTput
      p_new_kwh                IN OUT   NUMBER,
      p_new_read_val           IN OUT   NUMBER,
      p_new_read_type          IN OUT   NUMBER,
      p_new_err_code           IN OUT   NUMBER,
      p_force_zero_amnt        IN OUT   BOOLEAN,
      -- estimate reading correction options
      p_est_reading            IN OUT   NUMBER,
      p_force_est_correction   IN OUT   BOOLEAN
   )
   IS
      -- does not have previous history?
      has_empty_history      BOOLEAN := NVL (p_prev_read.itemkey, 0) = 0;
      -- has previous actual reading?
      has_prev_act_reading   BOOLEAN := NOT NVL (p_prev_act_read.itemkey, 0) =
                                                                              0;
      -- interval dates
      d1                     DATE;
      d2                     DATE;
      -- month distinction
      p_month_distiction     NUMBER;
   BEGIN
      -- Determine interval dates
      IF has_empty_history
      THEN
         d1 := TRUNC (p_acc_cr_date);
         d2 := TRUNC (p_curr_read_date);
      ELSE
         d1 := TRUNC (p_prev_read.itemdate);
         d2 := TRUNC (p_curr_read_date);
      END IF;

      -- Process reading

      -- 1. when meter does not exist or is damaged
      IF p_mt_type = mt_type_none OR NOT p_mt_stat OR NOT p_seal_stat
      THEN
         IF NOT p_is_cutted
         THEN
            p_new_read_type := oper_without_meter;
            p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
            p_new_err_code := err_01;
            p_new_read_val := 0;
         ELSE
            p_new_read_type := oper_without_meter;
            p_new_kwh := 0;
            p_new_err_code := err_02;
            p_new_read_val := 0;
         END IF;
      -- 2. previous was 'meter deinstall'
      ELSIF p_prev_read.billoperkey = oper_meter_deinstall
      THEN
         IF NOT p_is_cutted
         THEN
            --p_new_read_type := OPER_ESTIMATE;
            p_new_read_type := oper_without_meter;
            p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
            p_new_err_code := err_18;
            --p_new_read_val := derive_read_val(p_prev_read.READING, p_new_kwh,
            --  p_mt_coeff, p_mt_digits);
            -- @since 28/07/2006
            p_new_read_val := 0;
         ELSE
            p_new_read_type := oper_without_meter;
            p_new_kwh := 0;
            p_new_read_val := 0;
            p_new_err_code := err_19;
         END IF;
      -- 3. when previous was 'without meter'
      ELSIF p_prev_read.billoperkey IN
                                   (oper_without_meter, oper_notoperable_meter)
      THEN
         IF NOT p_is_cutted
         THEN
            p_new_read_type := oper_without_meter;
            p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
            p_new_read_val := p_prev_read.reading;
            p_new_err_code := err_20;
         ELSE
            p_new_read_type := oper_without_meter;
            p_new_kwh := 0;
            p_new_read_val := 0;
            p_new_err_code := err_21;
         END IF;
      -- 4. Reading value is not compatible with meter digits
      ELSIF p_curr_read_val > POWER (10, p_mt_digits)
      THEN
         IF NOT p_is_cutted
         THEN
            p_new_read_type := oper_estimate;
            p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
            p_new_read_val :=
               derive_read_val (p_prev_read.reading,
                                p_new_kwh,
                                p_mt_coeff,
                                p_mt_digits
                               );
            p_new_err_code := err_03;
         ELSE
            p_new_read_type := p_prev_read.billoperkey;
            p_new_read_val := p_prev_read.reading;
            p_new_kwh := 0;
            p_force_zero_amnt := TRUE;
            p_new_err_code := err_04;
         END IF;
      -- 5. Has not last actual reading
      ELSIF NOT has_prev_act_reading
      THEN
         IF NOT p_is_cutted
         THEN
            p_new_read_type := oper_control_reading;
            p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
            p_new_read_val := p_curr_read_val;

            IF has_prev_act_reading
            THEN
               p_new_err_code := err_05;
            ELSE
               p_new_err_code := err_16;
            END IF;
         ELSE
            p_new_read_type := oper_control_reading;
            p_new_kwh := 0;
            p_force_zero_amnt := TRUE;
            p_new_err_code := err_06;
            --p_new_read_val := 0;
            -- @since 28/07/2006
            p_new_read_val := p_curr_read_val;
         END IF;
      -- 6. reading OK!
      ELSE
         -- 6.1. deriver KWH charge

         -- 6.1.1. previous was estimate reading
         IF p_prev_read.billoperkey = oper_estimate
         THEN
            p_month_distiction :=
               ABS (MONTHS_BETWEEN (p_prev_act_read.itemdate, p_curr_read_date));

            -- Can not be recalculated:
            --IF
            --p_prev_act_read.ITEMDATE < CONTROLABLE_FROM

            -- @since 1-Jul-2006
            -- category was suppressed on recalculation: 3 month for comercial,
            -- 6 monthes for other categories
            IF (   (p_cust_cat_key IN (2, 3, 9, 10) AND p_month_distiction > 4)
                OR (p_month_distiction > 7)
               )
            THEN
               IF NOT p_is_cutted
               THEN
                  p_force_est_correction := FALSE;
                  p_new_read_type := oper_estimate;
                  p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
                  p_new_read_val :=
                     derive_read_val (p_prev_read.reading,
                                      p_new_kwh,
                                      p_mt_coeff,
                                      p_mt_digits
                                     );
                  p_new_err_code := err_17;
               ELSE
                  p_new_kwh := 0;
                  p_force_zero_amnt := TRUE;
                  p_force_est_correction := FALSE;
                  p_new_read_type := p_prev_read.billoperkey;
                  p_new_read_val := p_prev_read.reading;
                  p_new_err_code := err_22;
               END IF;
            -- Can be recalculated:
            ELSE
               p_force_est_correction := TRUE;
               p_est_reading :=
                  derive_read_val (p_prev_act_read.reading,
                                   p_curr_read_val,
                                   p_prev_act_read.itemdate,
                                   p_curr_read_date,
                                   p_prev_read.itemdate,
                                   p_mt_digits
                                  );
               p_new_kwh :=
                  derive_kwh (p_curr_read_val,
                              p_est_reading,
                              p_prev_act_read.reading,
                              p_mt_coeff,
                              p_mt_digits
                             );
               p_new_read_type := oper_reading;
            END IF;
         -- 6.1.2. previous was real reading
         ELSE
            p_new_read_type := oper_reading;
            p_new_kwh :=
               derive_kwh (p_curr_read_val,
                           p_prev_read.reading,
                           p_prev_act_read.reading,
                           p_mt_coeff,
                           p_mt_digits
                          );
         END IF;

         -- 6.2. Now check conditions for the case of the real reading

         -- 6.2.1. Not conencted
         IF p_is_cutted
         THEN
            -- When cuted, it is not normal to have any KWH charge, mark this as an
            -- error. If additionally KWH charge is greater than the maximal limit
            -- then force zero charge and take previous reading parameters.
            IF ABS (NVL (p_new_kwh, 0)) > min_kwh
            THEN
               p_new_err_code := err_07;

               IF ABS (NVL (p_new_kwh, 0)) > ABS (p_max_kwh)
               THEN
                  p_new_kwh := 0;
                  p_force_zero_amnt := TRUE;
                  p_force_est_correction := FALSE;
                  p_new_read_type := p_prev_read.billoperkey;
                  p_new_read_val := p_prev_read.reading;
               END IF;
            END IF;
         -- 6.2.2. Connected
         ELSE
            -- if KWH charge is greater than the maximal charge, then process as
            -- 'estimate' charging
            IF ABS (NVL (p_new_kwh, 0)) > p_max_kwh
            THEN
               p_new_err_code := err_08;
               p_new_read_type := oper_estimate;
               p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
               p_new_read_val :=
                  derive_read_val (p_prev_read.reading,
                                   p_new_kwh,
                                   p_mt_coeff,
                                   p_mt_digits
                                  );
               p_force_est_correction := FALSE;
            -- when minimal charge condition is violated (it is not critical
            -- condition) process this case as it was previosly calculated, but
            -- mark error to consider closely later
            ELSIF p_new_kwh < p_min_kwh
            THEN
               p_new_err_code := err_08;
            END IF;
         END IF;                      -- end of connected/disconnected condition
      END IF;                                       -- end of the main condition
   END;                        -- end of the 'calc_cycle_with_reading' procedure

   /**
    * Calculates cycle reading when current reading does not exist.
    * This procedure is called by 'calc_cycle_reading' procedure when
    * current reading does not exist.
    */
   PROCEDURE calc_cycle_without_reading (
      -- account information
      p_acc_inst_cp                     NUMBER,
      p_acc_cr_date                     DATE,
      p_is_cutted                       BOOLEAN,
      p_min_kwh                         NUMBER,
      p_max_kwh                         NUMBER,
      -- meter conditions
      p_mt_type                         NUMBER,
      p_mt_digits                       NUMBER,
      p_mt_coeff                        NUMBER,
      p_mt_stat                         BOOLEAN,
      p_seal_stat                       BOOLEAN,
      -- current reading information
      p_curr_read_date                  DATE,
      -- previous reading information
      p_prev_read                       item%ROWTYPE,
      p_prev_act_read                   item%ROWTYPE,
      -- procedure IN OUTput
      p_new_kwh                IN OUT   NUMBER,
      p_new_read_val           IN OUT   NUMBER,
      p_new_read_type          IN OUT   NUMBER,
      p_new_err_code           IN OUT   NUMBER,
      p_force_zero_amnt        IN OUT   BOOLEAN,
      -- estimate reading correction options
      p_est_reading            IN OUT   NUMBER,
      p_force_est_correction   IN OUT   BOOLEAN
   )
   IS
      -- does not have previous history?
      has_empty_history      BOOLEAN := NVL (p_prev_read.itemkey, 0) = 0;
      -- has previous actual reading?
      has_prev_act_reading   BOOLEAN := NOT NVL (p_prev_act_read.itemkey, 0) =
                                                                              0;
      -- interval dates
      d1                     DATE;
      d2                     DATE;
   BEGIN
      -- Determine interval dates
      IF has_empty_history
      THEN
         d1 := TRUNC (p_acc_cr_date);
         d2 := TRUNC (p_curr_read_date);
      ELSE
         d1 := TRUNC (p_prev_read.itemdate);
         d2 := TRUNC (p_curr_read_date);
      END IF;

      -- Process withIN OUT reading

      -- 1. When connected
      IF NOT p_is_cutted
      THEN
         IF     NOT (p_mt_type = mt_type_none OR NOT p_mt_stat
                     OR NOT p_seal_stat
                    )
            AND (    has_prev_act_reading
                 AND p_prev_read.billoperkey NOT IN
                        (oper_without_meter,
                         oper_notoperable_meter,
                         oper_meter_deinstall
                        )
                )
         THEN
            p_new_read_type := oper_estimate;
            p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
            p_new_read_val :=
               derive_read_val (p_prev_read.reading,
                                p_new_kwh,
                                p_mt_coeff,
                                p_mt_digits
                               );
         ELSE
            p_new_read_type := oper_without_meter;
            p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
            p_new_read_val := 0;
         END IF;
      -- 2. When disconnected
      ELSE
         IF    p_mt_type = mt_type_none
            OR p_prev_read.billoperkey = oper_meter_deinstall
         THEN
            p_new_read_type := oper_without_meter;
            p_new_read_val := 0;
         ELSE
            p_new_read_type := p_prev_read.billoperkey;
            p_new_read_val := p_prev_read.reading;
         END IF;

         p_new_kwh := 0;
         p_force_zero_amnt := TRUE;
         p_new_err_code := err_10;
      END IF;                        -- end of connected/disconnected conditions
   END;                     -- end of the 'calc_cycle_without_reading' procedure

   /**
    * This procedure is used by 'calc_kwh' function to process not-cycle reading
    * information.
    */
   PROCEDURE calc_not_cycle_reading (
      -- customer category key
      p_cust_cat_key                    NUMBER,
      -- account information
      p_acc_inst_cp                     NUMBER,
      p_acc_cr_date                     DATE,
      p_is_cutted                       BOOLEAN,
      -- meter conditions
      p_mt_digits                       NUMBER,
      p_mt_coeff                        NUMBER,
      -- current reading information
      p_curr_read_val                   NUMBER,
      p_curr_read_type                  NUMBER,
      p_curr_read_date                  DATE,
      p_curr_kwh                        NUMBER,
      p_curr_amnt                       NUMBER,
      -- previous reading information
      p_prev_read                       item%ROWTYPE,
      p_prev_act_read                   item%ROWTYPE,
      -- procedure output
      p_new_kwh                IN OUT   NUMBER,
      p_new_read_val           IN OUT   NUMBER,
      p_new_read_type          IN OUT   NUMBER,
      p_force_zero_amnt        IN OUT   BOOLEAN,
      -- estimate reading correction options
      p_est_reading            IN OUT   NUMBER,
      p_force_est_correction   IN OUT   BOOLEAN
   )
   IS
      -- does not have previous history?
      has_empty_history      BOOLEAN := NVL (p_prev_read.itemkey, 0) = 0;
      -- has previous actual reading?
      has_prev_act_reading   BOOLEAN := NOT NVL (p_prev_act_read.itemkey, 0) =
                                                                              0;
      -- exists or not current reading value?
      has_current_reading    BOOLEAN := NOT NVL (p_curr_read_val, 0) = 0;
      -- current operation category
      p_curr_oper_cat        NUMBER  := get_oper_cat (p_curr_read_type);
      -- interval dates
      d1                     DATE;
      d2                     DATE;
   BEGIN                                    -- begin of 'calc_not_cycle_reading'
      -- determine interval dates
      IF has_empty_history
      THEN
         d1 := p_acc_cr_date;
         d2 := p_curr_read_date;
      ELSE
         d1 := p_prev_read.itemdate;
         d2 := p_curr_read_date;
      END IF;

      -- Check reading parameters
      IF LENGTH (TRIM (TO_CHAR (TRUNC (p_curr_read_val)))) > p_mt_digits
      THEN
         raise_application_error
                    (-20000,
                     'Current read value is not compatible with metter digits.'
                    );
      END IF;

      -- 1. Audit reading
      IF p_curr_oper_cat = oper_cat_audit
      THEN
         p_new_kwh := 0;
         p_force_zero_amnt := TRUE;
      -- 2. Reading, Sale, Metter Deinstall, Cut, Repair, Balance
      ELSIF p_curr_read_type IN
              (oper_reading,
               oper_sale,
               oper_meter_deinstall,
               oper_cut,
               oper_repair,
               oper_balance
              )
      THEN
         -- when account does not have previous history or previous
         -- read type was 'meter deinstall':
         -- Charge using estimate charge and mark as 'control reading'
         IF has_empty_history OR p_prev_read.billoperkey = oper_meter_deinstall
         THEN
            p_new_read_type := oper_control_reading;
            p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
         -- when account has no previous actual reading or previous
         -- read type is 'withIN OUT meter' or 'not operable meter':
         -- Charge using estimate charge and mark as 'control reading'
         ELSIF    NOT has_prev_act_reading
               OR (p_prev_read.billoperkey IN
                                   (oper_without_meter, oper_notoperable_meter)
                  )
         THEN
            -- OLD VERSION
            --p_new_read_type := OPER_CONTROL_READING;
            --p_new_kwh := derive_est_kwh(d1, d1, p_acc_inst_cp);
            raise_application_error
               (-20000,
                'Use control reading, while previous was ''without meter'' reading.'
               );
         -- when account has previous actual reading, but previous was
         -- 'estimate' reading
         -- then derive reading value for the estimate reading date
         -- and charge KWH as between two real readings, also mark this
         -- situation for further estimate corrections
         ELSIF has_prev_act_reading AND p_prev_read.billoperkey = oper_estimate
         THEN
            p_force_est_correction := TRUE;
            p_est_reading :=
               derive_read_val (p_prev_act_read.reading,
                                p_curr_read_val,
                                p_prev_act_read.itemdate,
                                p_curr_read_date,
                                p_prev_read.itemdate,
                                p_mt_digits
                               );
            p_new_kwh :=
               derive_kwh (p_curr_read_val,
                           p_est_reading,
                           p_prev_act_read.reading,
                           p_mt_coeff,
                           p_mt_digits
                          );
         -- charge between two real readings
         ELSE
            p_new_kwh :=
               derive_kwh (p_curr_read_val,
                           p_prev_read.reading,
                           p_prev_act_read.reading,
                           p_mt_coeff,
                           p_mt_digits
                          );
         END IF;
      -- 3. Current reading is meter install
      ELSIF p_curr_read_type = oper_meter_install
      THEN
         -- when account is cutted, then do not charge nothing
         IF p_is_cutted
         THEN
            p_new_kwh := 0;
            p_force_zero_amnt := TRUE;
         -- when previous was deinstall, then charge nothing
         ELSIF p_prev_read.billoperkey = oper_meter_deinstall
         THEN
            IF TRUNC (d1) = TRUNC (d2)
            THEN
               p_new_kwh := 0;
               p_force_zero_amnt := TRUE;
            ELSE
               p_new_kwh := p_curr_kwh;
               p_force_zero_amnt := TRUE;
            END IF;                                    -- end of dates condition
         -- otherwise charge using installed capacity
         ELSE
            p_new_kwh := derive_est_kwh (d1, d2, p_acc_inst_cp);
         END IF;
      -- 4. Control reading
      ELSIF p_curr_read_type = oper_control_reading
      THEN
         -- old version: check something

         -- check control reading input parameters for high voltage customers
         -- group
         IF (   (    ABS (NVL (p_curr_amnt, 0)) > min_gel
                 AND ABS (NVL (p_curr_kwh, 0)) < min_kwh
                )
             OR (    ABS (NVL (p_curr_amnt, 0)) < min_gel
                 AND ABS (NVL (p_curr_kwh, 0)) > min_kwh
                )
            )
         THEN
            raise_application_error
                               (-20000,
                                'Leave kWh and Gel amounts empty or enter both'
                               );
         END IF;
      -- 5. Illegal individual reading operation category
      ELSE
         raise_application_error (-20000, 'Not supported operation.');
      END IF;                                    -- end of categories conditions
   END;                         -- end of the 'calc_not_cycle_reading' procedure

   /**
    * This is called for estimate corrections.
    */
   PROCEDURE process_estimate_correction (
      -- customer information
      p_customer                    NUMBER,
      p_account                     NUMBER,
      p_schedule                    NUMBER,
      -- meter info
      p_mt_digits                   NUMBER,
      p_mt_coeff                    NUMBER,
      -- reading information
      p_new_read_val                NUMBER,
      p_new_read_type               NUMBER,
      p_new_read_date               DATE,
      -- history
      p_prev_actual_read            item%ROWTYPE,
      p_prev_read                   item%ROWTYPE,
      -- resoponsible
      p_operator                    NUMBER,
      -- force on current
      p_force_on_current   IN OUT   BOOLEAN,
      p_total_corrected    IN OUT   NUMBER
   )
   IS
      p_processed_items   num_array;
   BEGIN
      -- Fill estimate corrections schema
      correct_estimates (p_account,
                         p_new_read_val,
                         p_new_read_date,
                         p_mt_digits,
                         p_mt_coeff,
                         p_prev_actual_read,
                         p_prev_read
                        );
      -- Process estiamate correction for itself
      process_my_estimate_correction (p_customer,
                                      p_account,
                                      p_new_read_date,
                                      p_schedule,
                                      p_prev_actual_read,
                                      p_prev_read,
                                      p_operator,
                                      p_force_on_current,
                                      p_total_corrected,
                                      p_processed_items
                                     );
      -- Process parent corrections
      process_parent_correction (p_account,
                                 p_processed_items,
                                 p_new_read_date,
                                 p_operator
                                );
   END;                        -- end of 'process_estimate_correction' procedure

   /**
    * Proccess parent corrections.
    */
   PROCEDURE process_parent_correction (
      p_account           NUMBER,
      p_processed_items   num_array,
      p_new_read_date     DATE,
      p_operator          NUMBER
   )
   IS
      p_index           NUMBER := 1;
      p_item            NUMBER;
      p_prnt_acc        NUMBER;
      p_prnt_cust       NUMBER;
      p_kwh_old         NUMBER;
      p_kwh_new         NUMBER;
      p_gel_old         NUMBER;
      p_gel_new         NUMBER;
      p_year            NUMBER;
      p_total_kwh_old   NUMBER;
      p_total_gel_old   NUMBER;
      p_total_kwh_new   NUMBER;
      p_total_gel_new   NUMBER;
      p_prev_year       NUMBER := -1;
      p_prev_acct       NUMBER := -1;
      p_prev_cust       NUMBER := -1;
   BEGIN                  -- begin of the 'process_parent_corrections' procedure
      -- Loop over all processed items
      WHILE p_index <= p_processed_items.COUNT
      LOOP
         -- get charge info
         charge_for_parent (p_processed_items (p_index),
                            p_kwh_old,
                            p_kwh_new,
                            p_gel_old,
                            p_gel_new,
                            p_year,
                            p_prnt_acc,
                            p_prnt_cust
                           );

         IF p_prev_year != p_year OR p_prev_acct != p_prnt_acc
         THEN
            IF ABS (NVL (p_total_kwh_old, 0)) > min_kwh
            THEN
               -- discharge
               p_item :=
                  populate_item (p_prnt_cust,
                                 p_prev_acct,
                                 NULL,
                                 NULL,
                                 get_correction_code (p_prev_year,
                                                      discharge_parent
                                                     ),
                                 p_operator,
                                 NULL,
                                 p_new_read_date,
                                 'prnt' || p_account,
                                 0,
                                 -p_total_kwh_old,
                                 -p_total_gel_old,
                                 0,
                                 NULL,
                                 item_rec_common,
                                 NULL,
                                 NULL,
                                 NULL
                                );

               IF ABS (NVL (p_total_kwh_new, 0)) > min_kwh
               THEN
                  -- recharge
                  p_item :=
                     populate_item (p_prnt_cust,
                                    p_prev_acct,
                                    NULL,
                                    NULL,
                                    get_correction_code (p_prev_year,
                                                         recharge_parent
                                                        ),
                                    p_operator,
                                    NULL,
                                    p_new_read_date,
                                    'prnt' || p_account,
                                    0,
                                    p_total_kwh_new,
                                    p_total_gel_new,
                                    0,
                                    NULL,
                                    item_rec_common,
                                    NULL,
                                    NULL,
                                    NULL
                                   );
               END IF;
            END IF;

            p_total_kwh_old := 0;
            p_total_gel_old := 0;
            p_total_kwh_new := 0;
            p_total_gel_new := 0;
         END IF;

         p_total_kwh_old := NVL (p_total_kwh_old, 0) + NVL (p_kwh_old, 0);
         p_total_gel_old := NVL (p_total_gel_old, 0) + NVL (p_gel_old, 0);
         p_total_kwh_new := NVL (p_total_kwh_new, 0) + NVL (p_kwh_new, 0);
         p_total_gel_new := NVL (p_total_gel_new, 0) + NVL (p_gel_new, 0);
         p_prev_year := NVL (p_year, p_prev_year);
         p_prev_acct := NVL (p_prnt_acc, p_prev_acct);
         p_prev_cust := NVL (p_prnt_cust, p_prev_cust);
         -- increase index
         p_index := p_index + 1;
      END LOOP;                               -- end of the processed items loop

      IF ABS (NVL (p_total_kwh_old, 0)) > min_kwh
      THEN
         -- discharge
         p_item :=
            populate_item (p_prev_cust,
                           p_prev_acct,
                           NULL,
                           NULL,
                           get_correction_code (p_prev_year, discharge_parent),
                           p_operator,
                           NULL,
                           p_new_read_date,
                           'prnt' || p_account,
                           0,
                           -p_total_kwh_old,
                           -p_total_gel_old,
                           0,
                           NULL,
                           item_rec_common,
                           NULL,
                           NULL,
                           NULL
                          );

         IF ABS (NVL (p_total_kwh_new, 0)) > min_kwh
         THEN
            -- recharge
            p_item :=
               populate_item (p_prev_cust,
                              p_prev_acct,
                              NULL,
                              NULL,
                              get_correction_code (p_prev_year, recharge_parent),
                              p_operator,
                              NULL,
                              p_new_read_date,
                              'prnt' || p_account,
                              0,
                              p_total_kwh_new,
                              p_total_gel_new,
                              0,
                              NULL,
                              item_rec_common,
                              NULL,
                              NULL,
                              NULL
                             );
         END IF;
      END IF;
   END;                      -- end of the 'process_parent_correction' procedure

   /**
    * Process estimate correction for the given account.
    */
   PROCEDURE process_my_estimate_correction (
      -- customer information
      p_customer                    NUMBER,
      p_account                     NUMBER,
      -- reading information
      p_new_read_date               DATE,
      p_schedule                    NUMBER,
      -- history
      p_prev_actual_read            item%ROWTYPE,
      p_prev_read                   item%ROWTYPE,
      -- resoponsible
      p_operator                    NUMBER,
      -- force on current
      p_force_on_current   IN OUT   BOOLEAN,
      p_total_corrected    IN OUT   NUMBER,
      p_processed_items    OUT      num_array
   )
   IS
      -- temporary item
      p_item_key      NUMBER;
      -- corrections
      p_corrections   tp_corrections;
      p_correction    tp_correction;
      -- index routine
      p_index         NUMBER;
      -- totals, needed when forcing on current
      p_total_init    NUMBER         := 0;
      p_total_corr    NUMBER         := 0;
   BEGIN              -- begin of the 'process_my_estimate_correction' procedure
      -- Initialize output
      p_force_on_current := FALSE;
      p_total_corrected := 0;
      -- Compare charges
      compare_cycle_charges (p_account,
                             p_new_read_date,
                             p_prev_read,
                             p_prev_actual_read,
                             p_corrections,
                             p_processed_items
                            );
      -- Correct percent subsidies
      p_index := 1;

      WHILE p_index <= p_corrections.COUNT
      LOOP
         p_correction := p_corrections (p_index);
         p_total_init := p_total_init + NVL (p_correction.discharge_kwh, 0);
         p_total_corr := p_total_corr + NVL (p_correction.recharge_kwh, 0);

         -- discharge percent subsidy
         IF ABS (NVL (p_correction.psubs_discharge_gel, 0)) > min_gel
         THEN
            p_item_key :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              get_correction_code (p_correction.YEAR,
                                                   discharge_psubs
                                                  ),
                              p_operator,
                              NULL,
                              p_new_read_date,
                              'sys',
                              0,
                              -p_correction.psubs_discharge_kwh,
                              -p_correction.psubs_discharge_gel,
                              0,
                              NULL,
                              item_rec_common,
                              NULL,
                              NULL,
                              NULL
                             );
         END IF;

         -- recharge percent subsidy
         IF ABS (NVL (p_correction.psubs_recharge_gel, 0)) > min_gel
         THEN
            p_item_key :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              get_correction_code (p_correction.YEAR,
                                                   recharge_psubs
                                                  ),
                              p_operator,
                              NULL,
                              p_new_read_date,
                              'sys',
                              0,
                              p_correction.psubs_recharge_kwh,
                              p_correction.psubs_recharge_gel,
                              0,
                              NULL,
                              item_rec_common,
                              NULL,
                              NULL,
                              NULL
                             );
         END IF;

         -- increase index
         p_index := p_index + 1;
      END LOOP;                              -- end of the percent subsides loop

      -- Check previous period charge
      IF ABS (p_total_init) < min_kwh
      THEN
         -- when corrected charge exists
         IF ABS (p_total_corr) > min_kwh
         THEN
            p_force_on_current := TRUE;
            p_total_corrected := p_total_corr;
         END IF;

         -- exit the routine
         RETURN;
      END IF;                             -- end of the previous period checking

      -- Process charges
      p_index := 1;

      WHILE p_index <= p_corrections.COUNT
      LOOP
         -- current correction
         p_correction := p_corrections (p_index);

         -- discharge previous period
         IF    (ABS (  NVL (p_correction.discharge_kwh, 0)
                     + NVL (p_correction.add_discharge_kwh, 0)
                    ) > min_kwh
               )
            OR (ABS (  NVL (p_correction.discharge_gel, 0)
                     + NVL (p_correction.add_discharge_gel, 0)
                    ) > min_gel
               )
         THEN
            p_item_key :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              get_correction_code (p_correction.YEAR,
                                                   discharge_plain
                                                  ),
                              p_operator,
                              NULL,
                              p_new_read_date,
                              'sys',
                              0,
                              - (  p_correction.discharge_kwh
                                 + p_correction.add_discharge_kwh
                                ),
                              - (  p_correction.discharge_gel
                                 + p_correction.add_discharge_gel
                                ),
                              0,
                              NULL,
                              item_rec_common,
                              NULL,
                              NULL,
                              NULL
                             );
         END IF;

         -- recharge previous period
         IF    (ABS (  NVL (p_correction.recharge_kwh, 0)
                     + NVL (p_correction.add_recharge_kwh, 0)
                    ) > min_kwh
               )
            OR (ABS (  NVL (p_correction.recharge_gel, 0)
                     + NVL (p_correction.add_recharge_gel, 0)
                    ) > min_gel
               )
         THEN
            p_item_key :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              get_correction_code (p_correction.YEAR,
                                                   recharge_plain
                                                  ),
                              p_operator,
                              NULL,
                              p_new_read_date,
                              'sys',
                              0,
                                p_correction.recharge_kwh
                              + p_correction.add_recharge_kwh,
                                p_correction.recharge_gel
                              + p_correction.add_recharge_gel,
                              0,
                              NULL,
                              item_rec_common,
                              NULL,
                              NULL,
                              NULL,
                              p_correction.recharge_det
                             );
         END IF;

         -- increase index
         p_index := p_index + 1;
      END LOOP;                                       -- end of the charges loop
   END;                 -- end of the 'process_my_estimate_correction' procedure

   /**
    * This procedure is called when estimate reading should be corrected with
    * derived reading and charge values.
    */
   PROCEDURE correct_estimate (
      est_item_key          NUMBER,
      est_derived_reading   NUMBER,
      est_derived_kwh       NUMBER
   )
   IS
      -- temporary parameters
      p_schedule                NUMBER;
      p_cycle                   NUMBER;
      p_account                 NUMBER;
      p_operation               NUMBER;
      p_add_kwh_kwh             NUMBER;
      p_add_percent_key         NUMBER;
      p_add_kwh_percent         NUMBER;
      p_subs_percent_key        NUMBER;
      p_subs_kwh_percent        NUMBER;
      p_prnt_item_key           NUMBER;
      p_prnt_subs_percent_key   NUMBER;
      p_prnt_subs_kwh_percent   NUMBER;
   BEGIN
      -- Get item information
      SELECT billoperkey, acckey, schedkey
        INTO p_operation, p_account, p_schedule
        FROM item
       WHERE itemkey = est_item_key;

      -- Get cycle for the schedule
      p_cycle := get_cycle (p_schedule);

      -- check item information
      IF p_operation != oper_estimate
      THEN
         raise_application_error (-20000, 'Not estimate item ' || est_item_key);
      ELSIF p_schedule IS NULL
      THEN
         raise_application_error (-20000,
                                     'No schedule defiend for the item '
                                  || est_item_key
                                 );
      END IF;                                -- end of item information checking

      -- Update ITEM_RELATION table for the main charge
      BEGIN
         -- populate ITEM_RELATION table
         INSERT INTO item_relation
                     (ir_item, ir_account, ir_cycle, ir_e_kwh,
                      ir_e_reading, ir_type
                     )
              VALUES (est_item_key, p_account, p_cycle, est_derived_kwh,
                      est_derived_reading, item_rec_valuable
                     );
      EXCEPTION
         WHEN OTHERS
         THEN
            -- update ITEM_RELATION table
            UPDATE item_relation
               SET ir_e_reading = est_derived_reading,
                   ir_e_kwh = est_derived_kwh,
                   ir_type = item_rec_valuable
             WHERE ir_item = est_item_key;
      END;

      -- Lookup % additional charges
      BEGIN
         -- look up for the (%) additional charges in this schedule
         SELECT itemkey, (unitvalue * unitnumb) / 100 * est_derived_kwh
           INTO p_add_percent_key, p_add_kwh_percent
           FROM (SELECT   i.itemkey, d.unitvalue, d.unitnumb
                     FROM billdetails d, billoperation b, item i
                    WHERE i.billoperkey = b.billoperkey
                      AND i.acckey = d.acckey
                      AND i.schedkey = p_schedule
                      AND d.billoperkey = b.billoperkey
                      AND d.acckey = p_account
                      AND b.opertpkey = oper_cat_add_charge
                      AND d.unittypekey = msr_unit_percent
                      AND (   (    d.active = 1
                               AND i.itemdate BETWEEN d.startdate AND d.enddate
                              )
                           OR (d.active = 0)
                          )
                 ORDER BY itemkey)
          WHERE ROWNUM = 1;

         -- Try to update item_relation record
         BEGIN
            -- try to insert into ITEM_RELATION table
            INSERT INTO item_relation
                        (ir_item, ir_account, ir_cycle,
                         ir_e_kwh, ir_type,
                         ir_item_from
                        )
                 VALUES (p_add_percent_key, p_account, p_cycle,
                         p_add_kwh_percent, item_rec_percent_charge,
                         est_item_key
                        );
         EXCEPTION
            WHEN OTHERS
            THEN
               -- update ITEM_RELATION table
               UPDATE item_relation
                  SET ir_e_kwh = p_add_kwh_percent
                WHERE ir_item = p_add_percent_key;
         END;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            p_add_kwh_percent := 0;
      END;

      -- Look up for the KWH additional charges -- they are unchanged
      BEGIN
         -- initialize additional KWH charges
         p_add_kwh_kwh := 0;

         -- calculate total KWH additional charges for this schedule
         FOR rec IN (SELECT i.*
                       FROM billdetails d, billoperation b, item i
                      WHERE i.billoperkey = b.billoperkey
                        AND i.acckey = d.acckey
                        AND i.schedkey = p_schedule
                        AND d.billoperkey = b.billoperkey
                        AND d.acckey = p_account
                        AND b.opertpkey = oper_cat_add_charge
                        AND d.unittypekey = msr_unit_kwh
                        AND (   (    d.active = 1
                                 AND i.itemdate BETWEEN d.startdate AND d.enddate
                                )
                             OR (d.active = 0)
                            ))
         LOOP
            p_add_kwh_kwh := p_add_kwh_kwh + rec.kwt;

            -- try to insert into ITEM_RELATION table
            BEGIN
               INSERT INTO item_relation
                           (ir_item, ir_account, ir_cycle, ir_type
                           )
                    VALUES (rec.itemkey, p_account, p_cycle, item_rec_valuable
                           );
            EXCEPTION
               WHEN OTHERS
               THEN
                  -- here is nothing to update
                  NULL;
            END;
         END LOOP;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            p_add_kwh_kwh := 0;
      END;

      -- Update percent subsidy
      BEGIN
         -- look up for the (%) subsidies in this schedule
         SELECT itemkey,
                  (unitvalue * unitnumb)
                / 100
                * (est_derived_kwh + p_add_kwh_kwh + p_add_kwh_percent)
           INTO p_subs_percent_key,
                p_subs_kwh_percent
           FROM (SELECT   i.itemkey, d.unitvalue, d.unitnumb
                     FROM billdetails d, billoperation b, item i
                    WHERE i.billoperkey = b.billoperkey
                      AND i.acckey = d.acckey
                      AND i.schedkey = p_schedule
                      AND d.billoperkey = b.billoperkey
                      AND d.acckey = p_account
                      AND b.opertpkey = oper_cat_subsidy
                      AND d.unittypekey = msr_unit_percent
                      AND (   (    d.active = 1
                               AND i.itemdate BETWEEN d.startdate AND d.enddate
                              )
                           OR (d.active = 0)
                          )
                 ORDER BY itemkey)
          WHERE ROWNUM = 1;

         -- Try to update
         BEGIN
            -- try to insert into ITEM_RELATION table
            INSERT INTO item_relation
                        (ir_item, ir_account,
                         ir_cycle, ir_e_kwh,
                         ir_type, ir_item_from
                        )
                 VALUES (p_subs_percent_key, p_account,
                         get_cycle (p_schedule), p_subs_kwh_percent,
                         item_rec_percent_charge, est_item_key
                        );
         EXCEPTION
            WHEN OTHERS
            THEN
               -- update ITEM_RELATION table
               UPDATE item_relation
                  SET ir_e_kwh = p_subs_kwh_percent,
                      ir_item_from = est_item_key
                WHERE ir_item = p_subs_percent_key;
         END;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            p_subs_kwh_percent := 0;
      END;
   END;                               -- end of the 'correct_estimate' procedure

   /**
    * This procedure derives real readings instead of estimate readings and than
    * calls 'correct_estimate' procedure in order to process this corrections.
    */
   PROCEDURE correct_estimates (
      p_account          NUMBER,
      p_new_reading      NUMBER,
      p_new_readdate     DATE,
      p_mt_digits        NUMBER,
      p_mt_coeff         NUMBER,
      p_last_real_read   item%ROWTYPE,
      p_last_read        item%ROWTYPE
   )
   IS
      p_prev_read      NUMBER := p_last_real_read.reading;
      p_derived_read   NUMBER;
      p_derived_kwh    NUMBER;
   BEGIN
      FOR rec IN (SELECT *
                    FROM item
                   WHERE billoperkey = oper_estimate
                     AND p_last_real_read.itemkey < itemkey
                     AND p_last_read.itemkey >= itemkey
                     AND acckey = p_account)
      LOOP
         -- derive read value
         p_derived_read :=
            derive_read_val (p_last_real_read.reading,
                             p_new_reading,
                             p_last_real_read.itemdate,
                             p_new_readdate,
                             rec.itemdate,
                             p_mt_digits
                            );
         -- derive kWh charge
         p_derived_kwh :=
            derive_kwh (p_derived_read,
                        p_prev_read,
                        p_prev_read,
                        p_mt_coeff,
                        p_mt_digits
                       );
         -- set previous reading value
         p_prev_read := p_derived_read;
         -- correct estimates
         correct_estimate (rec.itemkey, p_derived_read, p_derived_kwh);
      END LOOP;                                               -- end of the loop
   END;                              -- end of the 'correct_estimates' procedure

   /**
    * Simplified version of the Cycle Calculation procedure. May be used by outer
    * procedures in order to determine summary cycle charges.
    *
    * @since 1-Jul-2006
    */
   PROCEDURE calc_charge_for_cycle (
      p_cust_key         IN       NUMBER,
      p_cycle_key        IN       NUMBER,
      p_acc_key          IN       NUMBER,
      p_force_estimate   IN       BOOLEAN,
      p_total_kwh        OUT      NUMBER,
      p_total_gel        OUT      NUMBER
   )
   IS
      p_kwh               NUMBER;
      p_gel               NUMBER;
      p_total_kwh_psubs   NUMBER;
      p_total_gel_psubs   NUMBER;
      p_add_kwh_add       NUMBER;
      p_add_gel_add       NUMBER;
      p_processed_items   num_array       := num_array ();
      p_details           tp_item_details := tp_item_details ();
      p_add_details       tp_item_details := tp_item_details ();
   BEGIN
      calc_charge_for_cycle (p_cust_key,
                             p_cycle_key,
                             p_acc_key,
                             p_force_estimate,
                             p_kwh,
                             p_gel,
                             p_total_kwh_psubs,
                             p_total_gel_psubs,
                             p_add_kwh_add,
                             p_add_gel_add,
                             p_processed_items,
                             p_details,
                             p_add_details
                            );
      p_total_kwh :=
              NVL (p_kwh, 0) + NVL (p_total_kwh_psubs, 0)
              + NVL (p_add_kwh_add, 0);
      p_total_gel :=
              NVL (p_gel, 0) + NVL (p_total_gel_psubs, 0)
              + NVL (p_add_gel_add, 0);
   END;                          -- end of the 'calc_charge_for_cycle' procedure

   /**
    * Get charge for cycle (KWH and GEL) for the given customer. This charge is
    * total (cycle+not-cycle over all accounts) charge when step tariff is used
    * anytime during the cycle, otherwise this is simply total cyclic charge for
    * this account.
    *
    * When force parameter is <tt>TRUE</tt>, then charge for this account is
    * forcely calculated using initial charge parameters.
    */
   PROCEDURE calc_charge_for_cycle (
      p_cust_key          IN       NUMBER,
      p_cycle_key         IN       NUMBER,
      p_acc_key           IN       NUMBER,
      p_force_est         IN       BOOLEAN,
      p_total_kwh         OUT      NUMBER,
      p_total_gel         OUT      NUMBER,
      p_total_kwh_psubs   OUT      NUMBER,
      p_total_gel_psubs   OUT      NUMBER,
      p_add_kwh_add       OUT      NUMBER,
      p_add_gel_add       OUT      NUMBER,
      p_processed_items   IN OUT   num_array,
      p_details           IN OUT   tp_item_details,
      p_add_details       IN OUT   tp_item_details
   )
   IS
      p_use_complex   BOOLEAN := FALSE;
      --p_check_for_step BOOLEAN := FALSE;
      --p_cycle_checked  BOOLEAN := FALSE;

      -- schedule key
      p_schedule      NUMBER  := get_schedule (p_acc_key, p_cycle_key);
      -- initial/final dates for this cycle
      p_d1            DATE    := get_cycle_initial_date (p_acc_key, p_schedule);
      p_d2            DATE    := get_cycle_final_date (p_schedule);
   BEGIN                           -- begin of 'calc_charge_for_cycle' procedure
      -- Initialize output totals
      p_total_kwh := 0;
      p_total_gel := 0;
      p_total_kwh_psubs := 0;
      p_total_gel_psubs := 0;
      p_add_kwh_add := 0;
      p_add_gel_add := 0;

      -- adjust initial reading date, when needed
      IF p_d1 IS NULL
      THEN
         SELECT a.createdate
           INTO p_d1
           FROM ACCOUNT a
          WHERE acckey = p_acc_key;
      END IF;

      -- Has or not step tariff?

      -- when account has step-tariff, then use complex-cycle calculation routine
      p_use_complex := has_step_tariff (p_acc_key, p_d1, p_d2, oper_reading);

      -- Calculate total charges
      IF p_use_complex
      THEN
         -- calculate charge over all accounts for the full cycle
         calc_complex_cycle (p_cust_key,
                             p_cycle_key,
                             p_acc_key,
                             p_force_est,
                             p_total_kwh,
                             p_total_gel,
                             p_total_kwh_psubs,
                             p_total_gel_psubs,
                             p_add_kwh_add,
                             p_add_gel_add,
                             p_processed_items,
                             p_details
                            );
      ELSE
         -- calculate charge only for this cycle estimate
         calc_simple_est_cycle (p_cycle_key,
                                p_acc_key,
                                p_force_est,
                                p_total_kwh,
                                p_total_gel,
                                p_total_kwh_psubs,
                                p_total_gel_psubs,
                                p_add_kwh_add,
                                p_add_gel_add,
                                p_processed_items,
                                p_details,
                                p_add_details
                               );
      END IF;
   END;                          -- end of the 'calc_charge_for_cycle' procedure

   PROCEDURE calc_complex_cycle (
      p_cust_key          IN       NUMBER,
      p_cycle_key         IN       NUMBER,
      p_acc_key           IN       NUMBER,
      p_force_est         IN       BOOLEAN,
      p_total_kwh         IN OUT   NUMBER,
      p_total_gel         IN OUT   NUMBER,
      p_total_kwh_psubs   IN OUT   NUMBER,
      p_total_gel_psubs   IN OUT   NUMBER,
      p_add_kwh_add       OUT      NUMBER,
      p_add_gel_add       OUT      NUMBER,
      p_processed_items   IN OUT   num_array,
      p_details           IN OUT   tp_item_details
   )
   IS
      p_index                       NUMBER  := 1;
      p_force                       BOOLEAN := NVL (p_force_est, FALSE);
      p_schedule                    NUMBER;
      d1                            DATE;
      d2                            DATE;
      p_temp_kwh                    NUMBER;
      p_temp_gel                    NUMBER;
      p_array_size                  NUMBER  := 0;
      p_summary_gel                 NUMBER;
      p_summary_kwh                 NUMBER;
      p_psubs_summary_gel           NUMBER;
      p_psubs_summary_kwh           NUMBER;
      p_try_find_summary            BOOLEAN := TRUE;
      p_has_summary                 BOOLEAN := FALSE;
      p_has_psubs_summary           BOOLEAN := FALSE;
      -- @since 11/08/2006
      p_acc_creation                DATE;
      p_prev_cycle_last_enterdate   DATE;
      p_prev_cycle_last_item        NUMBER;
   BEGIN
      -- Initialize output parameters
      p_total_kwh := 0;
      p_total_gel := 0;
      p_total_kwh_psubs := 0;
      p_total_gel_psubs := 0;
      -- they are always ZERO
      p_add_kwh_add := 0;
      p_add_gel_add := 0;
      -- @since 11/08/2006 get cycle interval
      p_schedule := get_schedule (p_acc_key, p_cycle_key);
      d2 := get_cycle_final_date (p_schedule);

      SELECT a.createdate
        INTO p_acc_creation
        FROM ACCOUNT a
       WHERE a.acckey = p_acc_key;

      p_schedule := get_schedule (p_acc_key, p_cycle_key);
      d1 :=
         get_cycle_start_point (d2,
                                p_acc_key,
                                p_schedule,
                                p_acc_creation,
                                p_prev_cycle_last_enterdate,
                                p_prev_cycle_last_item
                               );
      -- Loop over all valuable item records for current account
      p_schedule := NULL;

      FOR rec IN (SELECT i.*, ir.ir_e_kwh, bop.opertpkey
                    FROM item i, item_relation ir, billoperation bop
                   WHERE i.itemkey = ir.ir_item
                     AND ir.ir_cycle = p_cycle_key
                     AND ir.ir_account = p_acc_key
                     AND ir.ir_type IN
                                   (item_rec_valuable, item_rec_percent_charge)
                     AND i.billoperkey = bop.billoperkey
                     AND
                         -- @since 11/08/2006 cut on enterdate to improve performance
                         i.enterdate > p_prev_cycle_last_enterdate - 30)
      LOOP
         -- @since 11/08/2006 consider only supported interval
         IF rec.itemdate >= d1
         THEN
            -- Initialize schedule, when it appears
            IF rec.schedkey IS NOT NULL AND p_schedule IS NULL
            THEN
               p_schedule := rec.schedkey;
            END IF;

            -- Try to find summaries

            -- try to find charge summary
            IF rec.schedkey IS NOT NULL AND p_force AND p_try_find_summary
            THEN
               p_try_find_summary := FALSE;

               -- Look up for charge summary
               BEGIN
                  SELECT kwt, amount
                    INTO p_summary_kwh, p_summary_gel
                    FROM item
                   WHERE billoperkey = oper_summary
                     AND schedkey = rec.schedkey
                     AND acckey = p_acc_key;

                  p_has_summary := TRUE;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     -- do not try any more
                     p_has_summary := FALSE;
               END;

               -- Look up for % subsidy summary
               BEGIN
                  SELECT kwt, amount
                    INTO p_psubs_summary_kwh, p_psubs_summary_gel
                    FROM item
                   WHERE billoperkey = oper_psubs_summary
                     AND schedkey = rec.schedkey
                     AND acckey = p_acc_key;

                  p_has_psubs_summary := TRUE;
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     p_has_psubs_summary := FALSE;
               END;
            END IF;

            -- Get charges

            -- get initial values
            IF p_force
            THEN
               p_temp_kwh := rec.kwt;
               p_temp_gel := rec.amount;
            -- try to get corrected values
            ELSE
               -- when correction exists
               IF rec.ir_e_kwh IS NOT NULL
               THEN
                  p_temp_kwh := rec.ir_e_kwh;
               -- otherwise
               ELSE
                  p_temp_kwh := rec.kwt;
               END IF;

               p_temp_gel := 0;
            END IF;                                -- end of the force condition

            -- populate processed items array (for parent calculations)
            IF p_force AND p_processed_items IS NOT NULL
            THEN
               p_array_size := p_processed_items.COUNT;
               p_processed_items.EXTEND;
               p_processed_items (p_array_size + 1) := rec.itemkey;
            END IF;

            -- Increase kWh charge

            -- percent subsidy
            -- [TODO]: consider this case, while subsidy may be both [kWh] and [%] for the cycle
            IF rec.opertpkey = oper_cat_subsidy
            THEN
               p_total_kwh_psubs := p_total_kwh_psubs + p_temp_kwh;
               p_total_gel_psubs := p_total_gel_psubs + p_temp_gel;
            -- other charges
            ELSE
               p_total_kwh := p_total_kwh + p_temp_kwh;
               p_total_gel := p_total_gel + p_temp_gel;
            END IF;
         END IF;                                   -- end of the cut on itemdate
      END LOOP;              -- end of the item records loop for current account

      -- when no total KWH charge
      IF ABS (NVL (p_total_kwh, 0)) < min_kwh
      THEN
         p_total_kwh := 0;
         p_total_gel := 0;
         p_total_kwh_psubs := 0;
         p_total_gel_psubs := 0;
         -- exit procedure
         RETURN;
      END IF;

      -- Get schedule for this account for the given cycle, if not yet defined
      IF p_schedule IS NULL
      THEN
         p_schedule := get_schedule (p_acc_key, p_cycle_key);
      END IF;

      -- Calculate totals

      -- when correction required
      IF NOT p_force
      THEN
         -- Get total gel charge

         -- calculate kWh charge
         p_details := tp_item_details ();
         p_total_gel :=
            calc_gel (p_acc_key,
                      oper_reading,
                      d1,
                      d2,
                      p_total_kwh,
                      FALSE,
                      p_details
                     );
         -- Get total gel charge for the percent subsidy

         -- calculate % subsidy
         p_total_gel_psubs := p_total_gel * p_total_kwh_psubs / p_total_kwh;
      -- when old parameters required
      ELSE
         -- has summary record
         IF p_has_summary
         THEN
            p_total_kwh := p_summary_kwh;
            p_total_gel := p_summary_gel;
         END IF;

         -- has % subsidy summary record
         IF p_has_psubs_summary
         THEN
            p_total_kwh_psubs := p_psubs_summary_kwh;
            p_total_gel_psubs := p_psubs_summary_gel;
         END IF;
      END IF;
   END;                             -- end of the 'calc_complex_cycle' procedure

   /**
    * Calculate simple estimate cycle.
    */
   PROCEDURE calc_simple_est_cycle (
      p_cycle_key         IN       NUMBER,
      p_acc_key           IN       NUMBER,
      p_force_est         IN       BOOLEAN,
      p_total_kwh         IN OUT   NUMBER,
      p_total_gel         IN OUT   NUMBER,
      p_total_kwh_psubs   IN OUT   NUMBER,
      p_total_gel_psubs   IN OUT   NUMBER,
      p_add_kwh_add       OUT      NUMBER,
      p_add_gel_add       OUT      NUMBER,
      p_processed_items   IN OUT   num_array,
      p_details           IN OUT   tp_item_details,
      p_add_details       IN OUT   tp_item_details
   )
   IS
      p_force              BOOLEAN := NVL (p_force_est, FALSE);
      p_schedule           NUMBER  := get_schedule (p_acc_key, p_cycle_key);
      p_est_item           NUMBER;
      p_est_kwh            NUMBER  := 0;
      p_est_kwh2           NUMBER  := 0;
      p_est_gel            NUMBER  := 0;
      p_add_percent_item   NUMBER;
      p_add_percent_kwh    NUMBER  := 0;
      p_add_percent_kwh2   NUMBER  := 0;
      p_add_percent_gel    NUMBER  := 0;
      p_add_kwh_kwh        NUMBER  := 0;
      p_add_kwh_gel        NUMBER  := 0;
      p_temp_kwh           NUMBER  := 0;
      p_subs_item          NUMBER;
      p_add_kwh_item       NUMBER;
      p_subs_kwh           NUMBER  := 0;
      p_subs_kwh2          NUMBER  := 0;
      p_subs_gel           NUMBER  := 0;
      p_array_size         NUMBER  := 0;
      --@since 11/08/2006
      p_acc_creation       DATE;
   BEGIN                       -- begin of the 'calc_simple_est_cycle' procedure
      -- Get account creation date

      --@since 11/08/2006
      SELECT a.createdate
        INTO p_acc_creation
        FROM ACCOUNT a
       WHERE acckey = p_acc_key;

      -- Initialize output parameters
      p_total_kwh := 0;
      p_total_gel := 0;
      p_total_kwh_psubs := 0;
      p_total_gel_psubs := 0;
      p_add_kwh_add := 0;
      p_add_gel_add := 0;

      -- Look up for the estimate charge
      BEGIN
         -- find charge values
         SELECT itemkey, kwt, amount
           INTO p_est_item, p_est_kwh, p_est_gel
           FROM item
          WHERE acckey = p_acc_key
            AND schedkey = p_schedule
            AND billoperkey = oper_estimate
            AND ROWNUM = 1;                                  -- force ONE record

         -- when need not force initial values
         IF NOT p_force                                      -- corrected values
         THEN
            -- get corrected KWH
            BEGIN
               SELECT ir_e_kwh
                 INTO p_est_kwh2
                 FROM item_relation
                WHERE ir_item = p_est_item;
            EXCEPTION
               WHEN OTHERS
               THEN
                  p_est_kwh2 := NULL;
            END;

            -- verify corrected values and derive GEL
            IF p_est_kwh2 IS NOT NULL
            THEN
               p_est_kwh := p_est_kwh2;
               p_est_gel :=
                  calc_gel_for_item (p_acc_key,
                                     p_acc_key,
                                     p_est_item,
                                     p_est_kwh,
                                     p_acc_creation,
                                     FALSE,
                                     FALSE,
                                     p_details
                                    );
            END IF;
         END IF;

         -- populate processed items array
         IF p_force
         THEN
            p_array_size := p_processed_items.COUNT;
            p_processed_items.EXTEND;
            p_processed_items (p_array_size + 1) := p_est_item;
         END IF;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            -- this procedure is only for the estimate readings, therefore exit
            -- when estimate reading can not be found
            RETURN;
      END;

      -- Look up for the % additional charges
      BEGIN
         -- get percent additional charges
         SELECT i.itemkey, i.kwt, i.amount
           INTO p_add_percent_item, p_add_percent_kwh, p_add_percent_gel
           FROM item i, billoperation b, billdetails d
          WHERE i.billoperkey = b.billoperkey
            AND i.acckey = d.acckey
            AND i.schedkey = p_schedule
            AND d.billoperkey = b.billoperkey
            AND d.acckey = p_acc_key
            AND b.opertpkey = oper_cat_add_charge
            AND d.unittypekey = msr_unit_percent
            AND (   (    d.enddate IS NOT NULL
                     AND i.itemdate BETWEEN d.startdate AND d.enddate
                    )
                 OR (d.enddate IS NULL)
                )
            AND ROWNUM = 1;                                  -- force ONE record

         -- when need not force initial values
         IF NOT p_force
         THEN
            -- get corrected KWH charge
            BEGIN
               SELECT ir_e_kwh
                 INTO p_add_percent_kwh2
                 FROM item_relation
                WHERE ir_item = p_add_percent_item;
            EXCEPTION
               WHEN OTHERS
               THEN
                  p_add_percent_kwh2 := NULL;
            END;

            IF p_add_percent_kwh2 IS NOT NULL
            THEN
               p_add_percent_kwh := p_add_percent_kwh2;

               IF ABS (NVL (p_est_kwh, 0)) > min_kwh
               THEN
                  p_add_percent_gel :=
                                      p_add_percent_kwh / p_est_kwh * p_est_gel;
               ELSE
                  p_add_percent_gel := 0;
               END IF;
            END IF;
         END IF;

         -- populate processed items array
         IF p_force
         THEN
            p_array_size := p_processed_items.COUNT;
            p_processed_items.EXTEND;
            p_processed_items (p_array_size + 1) := p_add_percent_item;
         END IF;
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      -- Look up additional KWH charges
      BEGIN
         p_add_kwh_kwh := 0;
         p_add_kwh_gel := 0;

         -- corrected values
         IF NOT p_force
         THEN
            -- get additional charges summary KWH
            FOR rec IN (SELECT i.*
                          FROM item i, billoperation b, billdetails d
                         WHERE i.billoperkey = b.billoperkey
                           AND i.acckey = d.acckey
                           AND i.schedkey = p_schedule
                           AND d.billoperkey = b.billoperkey
                           AND d.acckey = p_acc_key
                           AND b.opertpkey = oper_cat_add_charge
                           AND d.unittypekey = msr_unit_kwh
                           AND (   (    d.enddate IS NOT NULL
                                    AND i.itemdate BETWEEN d.startdate AND d.enddate
                                   )
                                OR (d.enddate IS NULL)
                               ))
            LOOP
               p_add_kwh_item := rec.itemkey;

               BEGIN
                  SELECT ir_e_kwh
                    INTO p_temp_kwh
                    FROM item_relation
                   WHERE ir_item = rec.itemkey;
               EXCEPTION
                  WHEN OTHERS
                  THEN
                     p_temp_kwh := rec.kwt;
               END;

               IF p_temp_kwh IS NULL
               THEN
                  p_temp_kwh := rec.kwt;
               END IF;

               p_add_kwh_kwh := p_add_kwh_kwh + p_temp_kwh;
            END LOOP;

            -- correct estimate value
            IF p_add_kwh_item IS NULL
            THEN
               p_add_kwh_item := p_est_item;
            END IF;

            -- calculate GEL for additional charges
            p_add_kwh_gel :=
               calc_gel_for_item (p_acc_key,
                                  p_acc_key,
                                  p_add_kwh_item,
                                  p_add_kwh_kwh,
                                  p_acc_creation,
                                  TRUE,
                                  FALSE,
                                  p_add_details
                                 );
         -- initial values
         ELSE
            FOR rec IN (SELECT i.*
                          FROM item i, billoperation b, billdetails d
                         WHERE i.billoperkey = b.billoperkey
                           AND i.acckey = d.acckey
                           AND i.schedkey = p_schedule
                           AND d.billoperkey = b.billoperkey
                           AND d.acckey = p_acc_key
                           AND b.opertpkey = oper_cat_add_charge
                           AND d.unittypekey = msr_unit_kwh
                           AND (   (    d.enddate IS NOT NULL
                                    AND i.itemdate BETWEEN d.startdate AND d.enddate
                                   )
                                OR (d.enddate IS NULL)
                               ))
            LOOP
               -- charge parameters
               p_add_kwh_kwh := p_add_kwh_kwh + rec.kwt;
               p_add_kwh_gel := p_add_kwh_gel + rec.amount;
               -- processed items
               p_array_size := p_processed_items.COUNT;
               p_processed_items.EXTEND;
               p_processed_items (p_array_size + 1) := rec.itemkey;
            END LOOP;
         END IF;                           -- end of initial/corrected condition
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            NULL;
      END;

      -- Look up % subsidy
      BEGIN
         -- get percent additional charges
         SELECT i.itemkey, i.kwt, i.amount
           INTO p_subs_item, p_subs_kwh, p_subs_gel
           FROM item i, billoperation b, billdetails d
          WHERE i.billoperkey = b.billoperkey
            AND i.acckey = d.acckey
            AND i.schedkey = p_schedule
            AND d.billoperkey = b.billoperkey
            AND d.acckey = p_acc_key
            AND b.opertpkey = oper_cat_subsidy
            AND d.unittypekey = msr_unit_percent
            AND (   (    d.enddate IS NOT NULL
                     AND i.itemdate BETWEEN d.startdate AND d.enddate
                    )
                 OR (d.enddate IS NULL)
                )
            AND ROWNUM = 1;                               -- force single record

         -- when need not force initial values
         IF NOT p_force
         THEN
            -- get corrected KWH
            BEGIN
               SELECT ir_e_kwh
                 INTO p_subs_kwh2
                 FROM item_relation
                WHERE ir_item = p_subs_item;
            EXCEPTION
               WHEN OTHERS
               THEN
                  p_subs_kwh2 := NULL;
            END;

            -- verify corrected values and derive GEL
            IF p_subs_kwh2 IS NOT NULL
            THEN
               p_subs_kwh := p_subs_kwh2;
               p_subs_gel :=
                    p_subs_kwh
                  / (  NVL (p_est_kwh, 0)
                     + NVL (p_add_percent_kwh, 0)
                     + NVL (p_add_kwh_kwh, 0)
                    )
                  * (  NVL (p_est_gel, 0)
                     + NVL (p_add_percent_gel, 0)
                     + NVL (p_add_kwh_gel, 0)
                    );
            END IF;
         END IF;
      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      -- Make totals
      p_total_kwh := p_est_kwh;
      p_total_gel := p_est_gel;
      p_total_kwh_psubs := NVL (p_subs_kwh, 0);
      p_total_gel_psubs := NVL (p_subs_gel, 0);
      p_add_kwh_add := NVL (p_add_percent_kwh, 0) + NVL (p_add_kwh_kwh, 0);
      p_add_gel_add := NVL (p_add_percent_gel, 0) + NVL (p_add_kwh_gel, 0);
   END;                          -- end of the 'calc_simple_est_cycle' procedure

   /**
    * Derive charge parameters (old and new) for parent.
    */
   PROCEDURE charge_for_parent (
      p_from_item   IN       NUMBER,
      p_kwh_old     OUT      NUMBER,
      p_kwh_new     OUT      NUMBER,
      p_gel_old     OUT      NUMBER,
      p_gel_new     OUT      NUMBER,
      p_year        OUT      NUMBER,
      p_prnt_acc    OUT      NUMBER,
      p_prnt_cust   OUT      NUMBER
   )
   IS
      p_child           NUMBER;
      p_child_kwh_old   NUMBER;
      p_child_kwh_new   NUMBER;
      d1                DATE;
      d2                DATE;
      p_details         tp_item_details;
      p_acc_creation    DATE;
   BEGIN                           -- begin of the 'charge_for_parent' procedure
      -- Initialize output parameters
      p_kwh_old := 0;
      p_kwh_new := 0;
      p_gel_old := 0;
      p_gel_new := 0;

      -- Try to calculate parent charges
      BEGIN
         -- get child info
         SELECT i.acckey, i.kwt, ir.ir_e_kwh, i.itemdate
           INTO p_child, p_child_kwh_old, p_child_kwh_new, d2
           FROM item i, item_relation ir
          WHERE ir.ir_item = i.itemkey AND i.itemkey = p_from_item;

         SELECT createdate
           INTO p_acc_creation
           FROM ACCOUNT
          WHERE acckey = p_child;

         -- adjust child charge
         p_child_kwh_new := NVL (p_child_kwh_new, p_child_kwh_old);

         -- discharge/recharge equal charges for parent?
         IF NOT correct_eq_prnt_charges
         THEN
            -- child old and new charges are the same
            IF ABS (NVL (p_child_kwh_old, 0) - NVL (p_child_kwh_new, 0)) <
                                                                        min_kwh
            THEN
               p_kwh_old := 0;
               p_kwh_new := 0;
               p_gel_old := 0;
               p_gel_new := 0;
               -- exit the procedure
               RETURN;
            END IF;
         END IF;

         -- lookup for the related parent charge
         SELECT i.acckey, i.custkey, i.kwt, i.amount
           INTO p_prnt_acc, p_prnt_cust, p_kwh_old, p_gel_old
           FROM item i, item_relation ir
          WHERE ir.ir_item = i.itemkey
            AND ir.ir_type = item_rec_parent_charge
            AND ir.ir_item_from = p_from_item;

         -- parent and child charges are not compatible
         IF ABS (p_child_kwh_old + p_kwh_old) > min_kwh
         THEN
            p_kwh_old := 0;
            p_kwh_new := 0;
            p_gel_old := 0;
            p_gel_new := 0;
            -- exit the procedure
            RETURN;
         END IF;

         -- new charge parameters
         p_kwh_new := -NVL (p_child_kwh_new, 0);
         p_gel_new :=
            calc_gel_for_item (p_child,
                               p_prnt_acc,
                               p_from_item,
                               p_kwh_new,
                               p_acc_creation,
                               FALSE,
                               FALSE,
                               p_details
                              );
         -- extract year
         p_year := EXTRACT (YEAR FROM d2);
      -- exit on exception
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            p_kwh_old := 0;
            p_kwh_new := 0;
            p_gel_old := 0;
            p_gel_new := 0;
            RETURN;
      END;
   END;                              -- end of the 'charge_for_parent' procedure

   /**
    * Compares cycle charges before and after correction.
    */
   PROCEDURE compare_cycle_charges (
      p_acc_key            IN       NUMBER,
      p_read_date          IN       DATE,
      p_prev_read          IN       item%ROWTYPE,
      p_prev_actual_read   IN       item%ROWTYPE,
      p_corrections        OUT      tp_corrections,
      p_processed_items    OUT      num_array
   )
   IS
      -- temporary parameters
      p_cust_key        NUMBER;
      p_add_kwh         NUMBER          := 0;
      p_add_gel         NUMBER          := 0;
      p_add_kwh_psubs   NUMBER          := 0;
      p_add_gel_psubs   NUMBER          := 0;
      p_add_kwh_add     NUMBER          := 0;
      p_add_gel_add     NUMBER          := 0;
      p_cycle           NUMBER;
      p_year            NUMBER          := 0;
      p_temp_year       NUMBER;
      p_year_changed    BOOLEAN;
      p_correction      tp_correction;
      p_index           NUMBER          := 0;
      p_details         tp_item_details;
      p_add_details     tp_item_details;
   BEGIN
      -- Get customer for this account
      SELECT custkey
        INTO p_cust_key
        FROM ACCOUNT
       WHERE acckey = p_acc_key;

      -- Create new corrections table
      p_corrections := tp_corrections ();
      -- Create numeric array of processed items
      p_processed_items := num_array ();

      -- Loop over all estimates
      FOR rec IN (SELECT   *
                      FROM item
                     WHERE billoperkey = oper_estimate
                       AND p_prev_actual_read.itemkey < itemkey
                       AND p_prev_read.itemkey >= itemkey
                       AND acckey = p_acc_key
                  ORDER BY itemkey)
      LOOP
         -- Get cycle
         p_cycle := get_cycle (rec.schedkey);
         -- Get year of the current operation
         p_temp_year := EXTRACT (YEAR FROM rec.itemdate);
         p_year_changed := p_temp_year != p_year;

         -- When year changed
         IF p_year_changed
         THEN
            p_index := p_index + 1;
            p_corrections.EXTEND;
            p_corrections (p_index) := p_correction;
            p_correction.YEAR := p_temp_year;
            p_correction.discharge_kwh := 0;
            p_correction.recharge_kwh := 0;
            p_correction.psubs_discharge_kwh := 0;
            p_correction.psubs_recharge_kwh := 0;
            p_correction.discharge_gel := 0;
            p_correction.recharge_gel := 0;
            p_correction.psubs_discharge_gel := 0;
            p_correction.psubs_recharge_gel := 0;
            p_correction.add_discharge_kwh := 0;
            p_correction.add_recharge_kwh := 0;
            p_correction.add_discharge_gel := 0;
            p_correction.add_recharge_gel := 0;
            p_correction.recharge_det := tp_item_details ();
         END IF;

         -- Calcualte initial charge values
         p_add_kwh := 0;
         p_add_gel := 0;
         p_add_kwh_psubs := 0;
         p_add_gel_psubs := 0;
         p_add_kwh_add := 0;
         p_add_gel_add := 0;
         p_details := NULL;
         p_add_details := NULL;
         calc_charge_for_cycle (p_cust_key,
                                p_cycle,
                                rec.acckey,
                                TRUE,
                                p_add_kwh,
                                p_add_gel,
                                p_add_kwh_psubs,
                                p_add_gel_psubs,
                                p_add_kwh_add,
                                p_add_gel_add,
                                p_processed_items,
                                p_details,
                                p_add_details
                               );
         p_correction.discharge_kwh := p_correction.discharge_kwh + p_add_kwh;
         p_correction.discharge_gel := p_correction.discharge_gel + p_add_gel;
         p_correction.psubs_discharge_kwh :=
                              p_correction.psubs_discharge_kwh + p_add_kwh_psubs;
         p_correction.psubs_discharge_gel :=
                              p_correction.psubs_discharge_gel + p_add_gel_psubs;
         p_correction.add_discharge_kwh :=
                                  p_correction.add_discharge_kwh + p_add_kwh_add;
         p_correction.add_discharge_gel :=
                                  p_correction.add_discharge_gel + p_add_gel_add;
         -- Calculate corrected charge values
         p_add_kwh := 0;
         p_add_gel := 0;
         p_add_kwh_psubs := 0;
         p_add_gel_psubs := 0;
         p_add_kwh_add := 0;
         p_add_gel_add := 0;
         p_details := tp_item_details ();
         p_add_details := tp_item_details ();
         calc_charge_for_cycle (p_cust_key,
                                p_cycle,
                                rec.acckey,
                                FALSE,
                                p_add_kwh,
                                p_add_gel,
                                p_add_kwh_psubs,
                                p_add_gel_psubs,
                                p_add_kwh_add,
                                p_add_gel_add,
                                p_processed_items,
                                p_details,
                                p_add_details
                               );
         p_correction.recharge_kwh := p_correction.recharge_kwh + p_add_kwh;
         p_correction.recharge_gel := p_correction.recharge_gel + p_add_gel;
         p_correction.psubs_recharge_kwh :=
                               p_correction.psubs_recharge_kwh + p_add_kwh_psubs;
         p_correction.psubs_recharge_gel :=
                               p_correction.psubs_recharge_gel + p_add_gel_psubs;
         p_correction.add_recharge_kwh :=
                                   p_correction.add_recharge_kwh + p_add_kwh_add;
         p_correction.add_recharge_gel :=
                                   p_correction.add_recharge_gel + p_add_gel_add;
         expand_item_details (p_details, p_correction.recharge_det);
         expand_item_details (p_add_details, p_correction.recharge_det);
         -- Finalize parameters
         p_year := p_temp_year;
         p_year_changed := FALSE;
      END LOOP;                                         -- end of estimates loop

      p_index := p_index + 1;
      p_corrections.EXTEND;
      p_corrections (p_index) := p_correction;
   END;                          -- end of the 'compare_cycle_charges' procedure

   /**
    * This procedure is used to populate ITEM table. It also makes records into
    * ITEM_RELATION table.
    */
   FUNCTION populate_item (
      p_customer          NUMBER,
      p_account           NUMBER,
      p_tariff            NUMBER,
      p_schedule          NUMBER,
      p_bill_operation    NUMBER,
      p_operator          NUMBER,
      p_sign_person       NUMBER,
      p_item_date         DATE,
      p_item_number       VARCHAR2,
      p_reading           NUMBER,
      p_kwh               NUMBER,
      p_gel               NUMBER,
      p_item_cat          NUMBER,
      p_note              NUMBER,
      p_record_category   NUMBER,
      p_from_item         NUMBER,
      p_child             NUMBER,
      p_child_cycle       NUMBER,
      p_details           tp_item_details := NULL
   )
      RETURN NUMBER
   IS
      -- newly inserted item key
      p_item_key      NUMBER;
      -- derived cycle value
      p_cycle         NUMBER;
      -- get reading category
      p_reading_cat   NUMBER;
      -- counter
      p_index         NUMBER;
      -- detail
      p_detail        tp_item_detail;
   BEGIN
      -- Check input parameters

      -- when record category is ITEM_REC_PARENT_CHARGE, then child and it's cycle
      -- should be defined
      IF     p_record_category = item_rec_parent_charge
         AND (p_child IS NULL OR p_child_cycle IS NULL)
      THEN
         raise_application_error
               (-20000,
                'Child or child''s cycle is not defined for the parent charge.'
               );
      -- when record cateogry is ITEM_REC_PERCENT_CHARGE, then percent charge
      -- producer item should be defined
      ELSIF p_record_category = item_rec_percent_charge AND p_from_item IS NULL
      THEN
         raise_application_error
                        (-20000,
                         'Producer item for the percent charge is not defined.'
                        );
      -- when record category is ITEM_REC_PARENT_PERCENT_CHARGE, then child, it's
      -- cycle and producer item should be defined
      ELSIF     p_record_category = item_rec_parent_percent_charge
            AND (p_child IS NULL OR p_child_cycle IS NULL OR p_from_item IS NULL
                )
      THEN
         raise_application_error
            (-20000,
                'Child, child''s cycle or producer item is not defined for the '
             || 'parent percent charge.'
            );
      END IF;                                -- end of input parameters checking

      -- Process insert into ITEM table

      -- insert given parameters and store newly inserted key
      INSERT INTO item
                  (acctarkey, billoperkey, acckey, custkey,
                   schedkey, perskey, signkey, itemdate,
                   itemnumber, reading, kwt, amount, enterdate, itemcatkey,
                   notekey
                  )
           VALUES (p_tariff, p_bill_operation, p_account, p_customer,
                   p_schedule, p_operator, p_sign_person, p_item_date,
                   p_item_number, p_reading, p_kwh, p_gel, SYSDATE, p_item_cat,
                   p_note
                  )
        RETURNING itemkey
             INTO p_item_key;

      -- Update account when real reading was inserted

      -- get reading category
      p_reading_cat := get_oper_cat (p_bill_operation);

      -- real reading condition
      IF p_reading_cat = oper_cat_reading AND NVL (p_reading, 0) > 0
      THEN
         UPDATE ACCOUNT
            SET last_reading = p_reading,
                last_read_date = p_item_date
          WHERE acckey = p_account;
      END IF;                                   -- end of real reading condition

      -- Derive cycle

      -- when schedule is not defined, then take
      -- default NONE cycle
      IF p_schedule IS NULL OR p_schedule < 1
      THEN
         p_cycle := cycle_none;
      -- otherwise derive cycle value from SCHEDULE
      -- table
      ELSE
         p_cycle := get_cycle (p_schedule);
      END IF;

      -- Popualte ITEM_RELATION table

      -- valuable item
      IF p_record_category = item_rec_valuable
      THEN
         INSERT INTO item_relation
                     (ir_item, ir_account, ir_cycle, ir_type
                     )
              VALUES (p_item_key, p_account, p_cycle, p_record_category
                     );
      -- percent charge item
      ELSIF p_record_category = item_rec_percent_charge
      THEN
         INSERT INTO item_relation
                     (ir_item, ir_account, ir_cycle, ir_item_from,
                      ir_type
                     )
              VALUES (p_item_key, p_account, p_cycle, p_from_item,
                      p_record_category
                     );
      -- parent record item
      ELSIF p_record_category = item_rec_parent_charge
      THEN
         INSERT INTO item_relation
                     (ir_item, ir_account, ir_cycle, ir_child, ir_child_cycle,
                      ir_item_from, ir_type
                     )
              VALUES (p_item_key, p_account, p_cycle, p_child, p_child_cycle,
                      p_from_item, p_record_category
                     );
      -- parent record percent item
      ELSIF p_record_category = item_rec_parent_percent_charge
      THEN
         INSERT INTO item_relation
                     (ir_item, ir_account, ir_cycle, ir_child, ir_child_cycle,
                      ir_item_from, ir_type
                     )
              VALUES (p_item_key, p_account, p_cycle, p_child, p_child_cycle,
                      p_from_item, p_record_category
                     );
      END IF;                                -- end of record category condition

      -- Populate details table
      add_item_details (p_item_key, p_details);
      -- Return calculation results
      RETURN p_item_key;
   END;                                   -- end of the 'populate_item' function

   /**
    * Add details for item.
    */
   PROCEDURE add_item_details (p_item_key NUMBER, p_details tp_item_details)
   IS
      p_index       NUMBER;
      p_detail      tp_item_detail;
      p_acctarkey   NUMBER;
   BEGIN
      IF p_details IS NOT NULL AND p_details.COUNT > 0
      THEN
         -- Get account tariff key and update BS.ITEM table with this value
         p_acctarkey := p_details (1).acctarkey;

         IF p_acctarkey IS NOT NULL
         THEN
            -- update item
            UPDATE item
               SET acctarkey = p_acctarkey
             WHERE itemkey = p_item_key;
         END IF;

         -- Initialize loop counter
         p_index := 1;

         -- Loop over all details
         WHILE p_index <= p_details.COUNT
         LOOP
            BEGIN
               -- detail
               p_detail := p_details (p_index);

               -- insert detail
               INSERT INTO item_details
                           (id_item, id_tar_key,
                            id_step_key, id_gel,
                            id_vat, id_kwh
                           )
                    VALUES (p_item_key, p_detail.tariff_key,
                            p_detail.tariff_step_key, NVL (p_detail.gel, 0),
                            NVL (p_detail.vat, 0), NVL (p_detail.kwh, 0)
                           );
            EXCEPTION
               WHEN OTHERS
               THEN
                  -- submit as warning
                  submit_warning (SQLCODE, SQLERRM);
            END;

            -- get next index
            p_index := p_index + 1;
         END LOOP;
      END IF;
   END;                                -- end of the 'add_item_details' function

   /**
    * Populate route store.
    */
   PROCEDURE populate_route_store (
      p_final_err_code    NUMBER,
      p_final_kwh         NUMBER,
      p_final_read_type   NUMBER,
      p_route_store_rec   route_store%ROWTYPE,
      p_final_read_val    NUMBER,
      p_send_to_print     BOOLEAN,
      p_full_kwh          NUMBER := NULL
   )
   IS
   BEGIN
      -- update ROUTE_STORE table
      IF NVL (p_final_err_code, 0) = 0
      THEN                                                -- when no error code
         -- update route store
         UPDATE route_store
            SET new_kwt = p_final_kwh,
                new_amount = p_full_kwh,
                new_balance = 0,
                new_rdtype = p_final_read_type
          WHERE rtstorekey = p_route_store_rec.rtstorekey;
      ELSE
         -- when sending to print, then additional record is not inserted
         IF p_send_to_print
         THEN
            -- update route store
            UPDATE route_store
               SET new_kwt = p_final_kwh,
                   new_amount = p_full_kwh,
                   new_balance = 0,
                   new_rdtype = p_final_read_type,
                   --new_reading = p_final_read_val,
                   ERROR_CODE = p_final_err_code,
                   status = 1
             WHERE rtstorekey = p_route_store_rec.rtstorekey;
         ELSE
            -- when not sending for print, then
            -- make new exception record

            -- mark as exception
            UPDATE route_store
               SET status = 1,
                   new_kwt = p_final_kwh,
                   new_amount = 0,
                   new_balance = 0,
                   ERROR_CODE = p_final_err_code
             WHERE rtstorekey = p_route_store_rec.rtstorekey;

            -- insert exception in a new row
            INSERT INTO route_store
                        (schedkey,
                         blockkey,
                         routekey, custkey,
                         acckey, accid,
                         accnumb, custname,
                         cur_mtstat,
                         cur_sealstat,
                         cur_digit,
                         cur_instcp,
                         cur_mtkoef,
                         cur_status,
                         cur_cut, new_rdtype, new_acctarkey,
                         new_reading, new_kwt, new_amount, new_balance,
                         new_readdate,
                         new_mtstat,
                         new_sealstat,
                         new_cut, new_note,
                         OPERATOR,
                         enterdate,
                         assistant,
                         editdate,
                         read_seq,
                         prv_reading,
                         prv_readdate,
                         prv_rdtype, status,
                         min_charge,
                         max_charge, ERROR_CODE,
                         cur_mttpkey,
                         prv_r_reading,
                         prv_r_readdate,
                         last_balance,
                         min_balance,
                         max_balance, tariff_todefault, estimate_parent,
                         createdate, tariffcheck_date, estimate_par_amt,
                         estimate_par_kwt
                        )
                 VALUES (p_route_store_rec.schedkey,
                         p_route_store_rec.blockkey,
                         p_route_store_rec.routekey, p_route_store_rec.custkey,
                         p_route_store_rec.acckey, p_route_store_rec.accid,
                         p_route_store_rec.accnumb, p_route_store_rec.custname,
                         p_route_store_rec.cur_mtstat,
                         p_route_store_rec.cur_sealstat,
                         p_route_store_rec.cur_digit,
                         p_route_store_rec.cur_instcp,
                         p_route_store_rec.cur_mtkoef,
                         p_route_store_rec.cur_status,
                         p_route_store_rec.cur_cut, p_final_read_type, NULL,
                                                              --tariff.acctarkey
                         p_final_read_val, p_final_kwh, NVL (p_full_kwh, 0), 0,
                                              -- amount --> FULL KWH and balance
                         p_route_store_rec.new_readdate,
                         p_route_store_rec.new_mtstat,
                         p_route_store_rec.new_sealstat,
                         p_route_store_rec.new_cut, p_route_store_rec.new_note,
                         p_route_store_rec.OPERATOR,
                         p_route_store_rec.enterdate,
                         p_route_store_rec.assistant,
                         p_route_store_rec.editdate,
                         p_route_store_rec.read_seq,
                         p_route_store_rec.prv_reading,
                         p_route_store_rec.prv_readdate,
                         p_route_store_rec.prv_rdtype, 2,
                         p_route_store_rec.min_charge,
                         p_route_store_rec.max_charge, p_final_err_code,
                         p_route_store_rec.cur_mttpkey,
                         p_route_store_rec.prv_r_reading,
                         p_route_store_rec.prv_r_readdate,
                         p_route_store_rec.last_balance,
                         p_route_store_rec.min_balance,
                         p_route_store_rec.max_balance, 0, 0,
                                                --tariff_default, and est parent
                         p_route_store_rec.createdate, NULL,   --tariffcheckdate
                                                            0,
                         0             --estimate_amt_parent,estimate_kwt_parent
                        );
         END IF;
      END IF;                               -- end of the ROUTE_STORE population
   END;                               -- end of 'populate_route_store' procedure

   /**
    * Process charge for the parent account.
    */
   PROCEDURE process_parent (
      p_parent           tp_parent,
      p_item_from        NUMBER,
      p_child_account    NUMBER,
      p_child_kwh        NUMBER,
      p_child_schedule   NUMBER,
      p_initial_date     DATE,
      p_final_date       DATE,
      p_operator         NUMBER
   )
   IS
      -- GEL charge
      p_gel           NUMBER;
      -- parent charge item key
      p_item          NUMBER;
      -- child cycle, derived from child schedule
      p_child_cycle   NUMBER;
      -- item details
      p_details       tp_item_details;
   BEGIN
      -- Check whether parent exists

      -- exit this procedure if parent exists
      IF NVL (p_parent.ACCOUNT, 0) = 0
      THEN
         RETURN;
      END IF;

      -- Get child cycle

      -- when schedule is not defined, than set as NONE cycle
      IF p_child_schedule IS NULL OR p_child_schedule < 1
      THEN
         p_child_cycle := cycle_none;
      -- derive schedule otherwise
      ELSE
         p_child_cycle := get_cycle (p_child_schedule);
      END IF;

      -- Derive the GEL charge
      p_details := tp_item_details ();
      p_gel :=
         calc_gel (p_parent.ACCOUNT,
                   oper_parent_charge,
                   p_initial_date,
                   p_final_date,
                   p_child_kwh,
                   TRUE,
                   p_details
                  );

      -- Populate item
      IF ABS (NVL (p_child_kwh, 0)) > min_kwh
      THEN
         p_item :=
            populate_item (p_parent.customer,
                           p_parent.ACCOUNT,
                           NULL,
                           NULL,
                           oper_parent_charge,
                           p_operator,
                           NULL,
                           p_final_date,
                           TO_CHAR (p_item_from),
                           0,
                           -p_child_kwh,
                           -p_gel,
                           0,
                           NULL,
                           item_rec_parent_charge,
                           p_item_from,
                           p_child_account,
                           p_child_cycle,
                           p_details
                          );
      END IF;

      -- process percent subsidies for parent
      process_percent_subsidy_parent (p_parent.customer,
                                      p_parent.ACCOUNT,
                                      -p_child_kwh,
                                      -p_gel,
                                      NULL,
                                      p_final_date,
                                      p_item,
                                      p_operator,
                                      p_child_account,
                                      p_child_cycle
                                     );
   END;                                 -- end of the 'process_parent' procedure

   /**
    * Process percent additional charge for the account.
    */
   PROCEDURE process_percent_add_charge (
      p_customer          NUMBER,
      p_account           NUMBER,
      p_kwh               NUMBER,
      p_gel               NUMBER,
      p_schedule          NUMBER,
      p_item_date         DATE,
      p_from_item         NUMBER,
      p_operator          NUMBER,
      -- output
      p_item_key    OUT   NUMBER,
      p_add_kwh     OUT   NUMBER,
      p_add_gel     OUT   NUMBER
   )
   IS
   BEGIN
      -- Initialize output parameters
      p_item_key := NULL;
      p_add_kwh := 0;
      p_add_gel := 0;

      -- return when nothing to charge
      IF ROUND (NVL (p_kwh, 0)) < min_kwh
      THEN
         RETURN;
      END IF;

      -- Loop over all percent additional charges

      -- NOTE: we use only one percent additional charge, while procedure
      -- for multiple percent additional charges is unknown

      -- only ONE additional percent charge is allowed, therefore this cycle
      -- is breaked after the first round

      -- Try to get additional charge value
      FOR rec IN (SELECT b.billoperkey operation,
                         d.unitvalue * d.unitnumb amount
                    FROM billdetails d, billoperation b
                   WHERE d.billoperkey = b.billoperkey
                     AND d.acckey = p_account
                     AND d.active = 0
                     AND d.unitnumb != 0
                     AND b.opertpkey = oper_cat_add_charge
                     AND d.unittypekey = msr_unit_percent)
      LOOP
         -- get additional charge value
         p_add_kwh := p_kwh * (rec.amount / 100);
         p_add_gel := p_gel * (rec.amount / 100);

         IF     ABS (NVL (p_add_kwh, 0)) > min_kwh
            AND (   (    ABS (NVL (p_add_gel, 0)) > min_gel
                     AND ABS (NVL (p_gel, 0)) > min_gel
                    )
                 OR (    ABS (NVL (p_add_gel, 0)) < min_gel
                     AND ABS (NVL (p_gel, 0)) < min_gel
                    )
                )
         THEN
            -- process additional charge
            p_item_key :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              rec.operation,
                              p_operator,
                              NULL,
                              p_item_date,
                              NULL,
                              0,
                              p_add_kwh,
                              p_add_gel,
                              0,
                              NULL,
                              item_rec_percent_charge,
                              p_from_item,
                              NULL,
                              NULL
                             );
            -- return after the first valuable charge
            RETURN;
         END IF;
      END LOOP;                                -- end of additional charges loop
   END;                     -- end of the 'process_percent_add_charge' procedure

   /**
    * Process KWH additional charge for the account.
    */
   PROCEDURE process_kwh_add_charge (
      p_customer           NUMBER,
      p_account            NUMBER,
      p_schedule           NUMBER,
      p_init_date          DATE,
      p_final_date         DATE,
      p_operator           NUMBER,
      p_has_flat           BOOLEAN,
      -- output
      p_item_key     OUT   NUMBER,
      p_add_kwh      OUT   NUMBER,
      p_add_gel      OUT   NUMBER
   )
   IS
      p_add_single              NUMBER          := 0;
      p_add_single_gel          NUMBER          := 0;
      p_details                 tp_item_details;
      p_prev_month_days_count   NUMBER;
   BEGIN
      -- Initialize output parameters
      p_item_key := NULL;
      p_add_kwh := 0;
      p_add_gel := 0;

      -- Loop over all KWH additional charges

      -- Try to get additional charge value
      FOR rec IN (SELECT b.billoperkey operation,
                         d.unitvalue * d.unitnumb amount
                    FROM billdetails d, billoperation b
                   WHERE d.billoperkey = b.billoperkey
                     AND d.acckey = p_account
                     AND d.active = 0
                     AND d.unitnumb != 0
                     AND b.opertpkey = oper_cat_add_charge
                     AND d.unittypekey = msr_unit_kwh)
      LOOP
         -- get additional charge value
         p_add_single := ROUND (rec.amount);

         -- Normalize additional charges, when required
         IF normalize_add_charges
         THEN
            -- normalize on previous month days count
            IF normalize_on_prev_month
            THEN
               p_prev_month_days_count :=
                     EXTRACT (DAY FROM LAST_DAY (ADD_MONTHS (p_final_date, -1)));
               p_add_single :=
                    p_add_single
                  / p_prev_month_days_count
                  * (p_final_date - p_init_date);
            -- normalize on fixed normalization period
            ELSE
               p_add_single :=
                    p_add_single
                  / normalization_period
                  * (p_final_date - p_init_date);
            END IF;
         END IF;

         p_add_kwh := p_add_kwh + p_add_single;
         p_add_single_gel := 0;
         p_details := NULL;

         IF p_has_flat
         THEN
            p_details := tp_item_details ();
            p_add_single_gel :=
               calc_gel (p_account,
                         rec.operation,
                         p_init_date,
                         p_final_date,
                         p_add_single,
                         FALSE,
                         p_details
                        );
            p_add_gel := p_add_gel + p_add_single_gel;
         END IF;

         -- process additional charge
         p_item_key :=
            populate_item (p_customer,
                           p_account,
                           NULL,
                           p_schedule,
                           rec.operation,
                           p_operator,
                           NULL,
                           p_final_date,
                           NULL,
                           0,
                           p_add_single,
                           p_add_single_gel,
                           0,
                           NULL,
                           item_rec_valuable,
                           NULL,
                           NULL,
                           NULL,
                           p_details
                          );
      END LOOP;                                -- end of additional charges loop
   END;                         -- end of the 'process_kwh_add_charge' procedure

   /**
    * Process KWH additional subsidy for the account.
    */
   PROCEDURE process_kwh_subsidy (
      p_customer           NUMBER,
      p_account            NUMBER,
      p_schedule           NUMBER,
      p_init_date          DATE,
      p_final_date         DATE,
      p_operator           NUMBER,
      p_has_flat           BOOLEAN,
      -- output
      p_item_key     OUT   NUMBER,
      p_add_kwh      OUT   NUMBER,
      p_add_gel      OUT   NUMBER
   )
   IS
      p_add_single       NUMBER          := 0;
      p_add_single_gel   NUMBER          := 0;
      p_details          tp_item_details;
   BEGIN
      -- do not process when not FLAT tariff
      IF NOT p_has_flat
      THEN
         RETURN;
      END IF;

      -- Initialize output parameters
      p_item_key := NULL;
      p_add_kwh := 0;
      p_add_gel := 0;

      -- Loop over all KWH subsidies

      -- Try to get subsidies
      FOR rec IN (SELECT b.billoperkey operation,
                         d.unitvalue * d.unitnumb amount
                    FROM billdetails d, billoperation b
                   WHERE d.billoperkey = b.billoperkey
                     AND d.acckey = p_account
                     AND d.active = 0
                     AND d.unitnumb != 0
                     AND b.opertpkey = oper_cat_subsidy
                     AND d.unittypekey = msr_unit_kwh)
      LOOP
         -- get additional charge value
         p_add_single := rec.amount;
         p_add_kwh := p_add_kwh + p_add_single;
         p_details := tp_item_details ();
         p_add_single_gel :=
            calc_gel (p_account,
                      rec.operation,
                      p_init_date,
                      p_final_date,
                      -p_add_single,
                      FALSE,
                      p_details,
                      TRUE
                     );
         p_add_gel := p_add_gel - p_add_single_gel;
         -- process KWH subsidy
         -- it is ALWAYS common record and can not be recharged!
         p_item_key :=
            populate_item (p_customer,
                           p_account,
                           NULL,
                           p_schedule,
                           rec.operation,
                           p_operator,
                           NULL,
                           p_final_date,
                           NULL,
                           0,
                           p_add_single,
                           -p_add_single_gel,
                           0,
                           NULL,
                           item_rec_common,
                           NULL,
                           NULL,
                           NULL,
                           p_details
                          );
      END LOOP;                                -- end of additional charges loop
   END;                         -- end of the 'process_kwh_add_charge' procedure

   /**
    * Process percent subsidy.
    */
   PROCEDURE process_percent_subsidy (
      p_customer          NUMBER,
      p_account           NUMBER,
      p_kwh               NUMBER,
      p_gel               NUMBER,
      p_schedule          NUMBER,
      p_item_date         DATE,
      p_from_item         NUMBER,
      p_operator          NUMBER,
      -- output
      p_item_key    OUT   NUMBER,
      p_subs_kwh    OUT   NUMBER,
      p_subs_gel    OUT   NUMBER
   )
   IS
   BEGIN
      -- Initialize output parameters
      p_item_key := NULL;
      p_subs_kwh := 0;
      p_subs_gel := 0;

      -- Loop over all percent subsidies

      -- only ONE percent subsidy is allowed, therefore this cycle is breaked
      -- after the first round

      -- Try to get percent subsidy value
      FOR rec IN (SELECT b.billoperkey operation,
                         d.unitvalue * d.unitnumb amount
                    FROM billdetails d, billoperation b
                   WHERE d.billoperkey = b.billoperkey
                     AND d.acckey = p_account
                     AND d.active = 0
                     AND d.unitnumb != 0
                     AND b.opertpkey = oper_cat_subsidy
                     AND d.unittypekey = msr_unit_percent)
      LOOP
         -- get value of subsidy kWh
         p_subs_kwh := p_kwh * (rec.amount / 100);
         p_subs_gel := p_gel * (rec.amount / 100);

         -- process additional charge
         IF     ABS (NVL (p_subs_kwh, 0)) > min_kwh
            AND (   (    ABS (NVL (p_subs_gel, 0)) > min_gel
                     AND ABS (NVL (p_gel, 0)) > min_gel
                    )
                 OR (    ABS (NVL (p_subs_gel, 0)) < min_gel
                     AND ABS (NVL (p_gel, 0)) < min_gel
                    )
                )
         THEN
            p_item_key :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              rec.operation,
                              p_operator,
                              NULL,
                              p_item_date,
                              NULL,
                              0,
                              p_subs_kwh,
                              p_subs_gel,
                              0,
                              NULL,
                              item_rec_percent_charge,
                              p_from_item,
                              NULL,
                              NULL
                             );
            -- return after the first valuable subsidy
            RETURN;
         END IF;
      END LOOP;                                -- end of additional charges loop
   END;                        -- end of the 'process_percent_subsidy' procedure

   /**
    * Process gel subsidies/additional charges.
    */
   PROCEDURE process_gel_charges (
      p_customer    NUMBER,
      p_account     NUMBER,
      p_schedule    NUMBER,
      p_item_date   DATE,
      p_operator    NUMBER
   )
   IS
      item_key   NUMBER;
   BEGIN
      -- Loop over all GEL subsidies/additional charges

      -- Try to get GEL subsidies/additional charges
      FOR rec IN (SELECT b.billoperkey operation,
                         d.unitvalue * d.unitnumb amount
                    FROM billdetails d, billoperation b
                   WHERE d.billoperkey = b.billoperkey
                     AND d.acckey = p_account
                     AND d.active = 0
                     AND d.unitnumb != 0
                     AND b.opertpkey IN (oper_cat_add_charge, oper_cat_subsidy)
                     AND d.unittypekey = msr_unit_gel)
      LOOP
         -- process GEL subsidy/additional charges
         item_key :=
            populate_item (p_customer,
                           p_account,
                           NULL,
                           p_schedule,
                           rec.operation,
                           p_operator,
                           NULL,
                           p_item_date,
                           NULL,
                           0,
                           0,
                           rec.amount,
                           0,
                           NULL,
                           item_rec_common,
                           NULL,
                           NULL,
                           NULL
                          );
      END LOOP;                  -- end of GEL subsidies/additional charges loop
   END;                            -- end of the 'process_gel_charges' procedure

   /**
    * Proccess percent subsidy for the parent.
    */
   PROCEDURE process_percent_subsidy_parent (
      p_customer      NUMBER,
      p_account       NUMBER,
      p_kwh           NUMBER,
      p_gel           NUMBER,
      p_schedule      NUMBER,
      p_item_date     DATE,
      p_from_item     NUMBER,
      p_operator      NUMBER,
      p_child         NUMBER,
      p_child_cycle   NUMBER
   )
   IS
      -- temporary
      item_key     NUMBER;
      p_subs_kwh   NUMBER := 0;
      p_subs_gel   NUMBER := 0;
   BEGIN
      -- Loop over all percent subsidies

      -- only ONE percent subsidy is allowed, therefore this cycle is breaked
      -- after the first round

      -- Try to get percent subsidy value
      FOR rec IN (SELECT b.billoperkey operation,
                         d.unitvalue * d.unitnumb amount
                    FROM billdetails d, billoperation b
                   WHERE d.billoperkey = b.billoperkey
                     AND d.acckey = p_account
                     AND d.active = 0
                     AND d.unitnumb != 0
                     AND b.opertpkey = oper_cat_subsidy
                     AND d.unittypekey = msr_unit_percent)
      LOOP
         p_subs_kwh := p_kwh * (rec.amount / 100);
         p_subs_gel := p_gel * (rec.amount / 100);

         -- process additional charge
         IF     ABS (NVL (p_subs_kwh, 0)) > min_kwh
            AND (   (    ABS (NVL (p_subs_gel, 0)) > min_gel
                     AND ABS (NVL (p_gel, 0)) > min_gel
                    )
                 OR (    ABS (NVL (p_subs_gel, 0)) < min_gel
                     AND ABS (NVL (p_gel, 0)) < min_gel
                    )
                )
         THEN
            -- process additional charge
            item_key :=
               populate_item (p_customer,
                              p_account,
                              NULL,
                              p_schedule,
                              rec.operation,
                              p_operator,
                              NULL,
                              p_item_date,
                              NULL,
                              0,
                              p_subs_kwh,
                              p_subs_gel,
                              0,
                              NULL,
                              item_rec_parent_percent_charge,
                              p_from_item,
                              p_child,
                              p_child_cycle
                             );
            -- return after the first valuable subsidy
            RETURN;
         END IF;
      END LOOP;                                -- end of additional charges loop
   END;                   -- end of the 'process_percent_subsidy_flat' procedure

   /**
    * This function is not used by this package.
    *
    * It calculates GEL charge for the current account tariff.
    *
    * @since 15-Jun-2006
    */
   FUNCTION calc_gel_for_natia (
      p_account         IN   NUMBER,
      p_charged_kwh     IN   NUMBER,
      p_charge_period   IN   NUMBER
   )
      RETURN NUMBER
   IS
      p_tariff    NUMBER;
      p_details   tp_item_details := tp_item_details ();
   BEGIN
      p_tariff := get_tariff_for_date (p_account, TRUNC (SYSDATE), 1);
      RETURN calc_gel (p_tariff,
                       p_charged_kwh,
                       p_charge_period,
                       TRUNC (SYSDATE),
                       FALSE,
                       p_details,
                       FALSE
                      );
   END;                              -- end of the 'calc_gel_for_natia' function

   FUNCTION calc_gel (
      p_tar_key         IN   NUMBER,
      p_charged_kwh     IN   NUMBER,
      p_charge_period   IN   NUMBER,
      p_d2              IN   DATE
   )
      RETURN NUMBER
   IS
      p_details   tp_item_details := tp_item_details ();
   BEGIN
      RETURN calc_gel (p_tar_key,
                       p_charged_kwh,
                       p_charge_period,
                       p_d2,
                       FALSE,
                       p_details,
                       FALSE
                      );
   END;

   /**
    * Calculate GEL amount using charged KWHs and tariff
    * key.
    */
   FUNCTION calc_gel (
      p_tar_key         IN       NUMBER,
      p_charged_kwh     IN       NUMBER,
      p_charge_period   IN       NUMBER,
      p_d2              IN       DATE,
      p_force_flat      IN       BOOLEAN := FALSE,
      p_details         IN OUT   tp_item_details,
      p_negative        IN       BOOLEAN := FALSE
   )
      RETURN NUMBER
   IS
   BEGIN
      -- Redirect call to ovveriden function
      IF p_force_flat OR NOT tariff_manager.has_step_structure (p_tar_key)
      THEN
         RETURN calc_gel (get_tar_comp_flat (p_tar_key),
                          NULL,
                          p_charged_kwh,
                          p_charge_period,
                          p_d2,
                          TRUE,
                          p_details,
                          p_negative
                         );
      ELSE
         RETURN calc_gel (get_tar_comp_flat (p_tar_key),
                          get_tar_comp_step (p_tar_key),
                          p_charged_kwh,
                          p_charge_period,
                          p_d2,
                          FALSE,
                          p_details,
                          p_negative
                         );
      END IF;
   END;                                         -- end of "calc_amount" function

   /**
    * Calculate GEL amount using charged KWHs and tariff
    * key.
    */
   FUNCTION calc_gel (
      p_tar_flat        IN       tarcomp%ROWTYPE,
      p_tar_steps       IN       tp_tar_steps,
      p_charged_kwh     IN       NUMBER,
      p_charge_period   IN       NUMBER,
      p_d2              IN       DATE,
      p_force_flat      IN       BOOLEAN := FALSE,
      p_details         IN OUT   tp_item_details,
      p_negative        IN       BOOLEAN := FALSE
   )
      RETURN NUMBER
   IS
      -- calculation temporary parameters
      p_index              NUMBER                := 1;
      p_amount             NUMBER                := 0;
      p_fit                BOOLEAN               := FALSE;
      p_step               tariff_step%ROWTYPE;
      p_interval           NUMBER;
      p_remaining_kwh      NUMBER                := p_charged_kwh;
      p_step_portion       NUMBER                := 0;
      p_corrected_period   NUMBER;
      p_tar_key            NUMBER;
      p_is_camel_tariff    BOOLEAN;
      p_vat_charge         NUMBER                := 0;
      p_kwh                NUMBER;
      p_new_detail         tp_item_detail;
   BEGIN                                     -- begin of the 'calc_gel' function
      -- Check charged KWHs

      --   return zero amount if charged KWHs are empty or
      --   zero raise error when charged KWHs is less then
      --   zero
      IF ABS (NVL (p_charged_kwh, 0)) < min_kwh
      THEN
         RETURN 0;
      ELSIF p_charged_kwh < 0 AND NOT p_force_flat
      THEN
         raise_application_error
                           (-20000,
                            'Amount of charged KWHs can not be less then zero.'
                           );
      END IF;

      -- Check current tariff structure

      -- raise error if neither steps nor flat tariff are not defined if steps
      -- are not defined or <tt>p_force_flat</tt> parameter=<tt>TRUE</tt>,
      -- then calculate using flat tariff
      IF p_tar_steps IS NULL OR p_tar_steps.COUNT = 0
      THEN
         IF NOT p_force_flat AND p_tar_flat.amount IS NULL
         THEN
            raise_application_error
                             (-20000,
                              'Steps are not defined and Flat Tariff is empty.'
                             );
         ELSE
            p_amount := p_tar_flat.amount * p_charged_kwh;
            p_new_detail.tariff_key := p_tar_flat.compkey;
            p_new_detail.tariff_step_key := NULL;

            IF p_negative
            THEN
               p_new_detail.gel := -p_amount;
               p_new_detail.vat :=
                  -tariff_manager.get_vat_charge (p_d2,
                                                  p_d2,
                                                  p_tar_flat.compkey,
                                                  p_amount
                                                 );
               p_new_detail.kwh := -p_charged_kwh;
            ELSE
               p_new_detail.gel := p_amount;
               p_new_detail.vat :=
                  tariff_manager.get_vat_charge (p_d2,
                                                 p_d2,
                                                 p_tar_flat.compkey,
                                                 p_amount
                                                );
               p_new_detail.kwh := p_charged_kwh;
            END IF;

            expand_item_details (p_new_detail, p_details);
            RETURN p_amount;
         END IF;
      END IF;

      -- Check calculation period

      -- raise error when period is unknown, zero or less
      -- the zero
      IF NVL (p_charge_period, 1) < 0
      THEN
         raise_application_error
                        (-20000,
                         'Amount can not be calculated for the negative period'
                        );
      END IF;

      -- adjust period
      IF NVL (p_charge_period, 1) = 0
      THEN
         p_corrected_period := 1;
      END IF;

      -- Determine whether is a camel tariff

      -- get tariff key
      p_tar_key := p_tar_steps (1).ts_tar_key;
      -- check camel structure
      p_is_camel_tariff := tariff_manager.is_camel_tariff (p_tar_key);

      -- Looping over all steps
      WHILE p_tar_steps.EXISTS (p_index) AND NOT p_fit
      LOOP
         -- get next step
         p_step := p_tar_steps (p_index);

         -- NOT 'Camel' structure step-tariff
         IF NOT p_is_camel_tariff
         THEN
            IF    p_step.ts_upper_bnd IS NULL
               OR ROUND (  p_step.ts_upper_bnd
                         * NVL (p_charge_period, 1)
                         / normalization_period,
                         3
                        ) >= ROUND (p_charged_kwh, 3)
            THEN
               p_kwh := p_charged_kwh;
               p_step_portion := p_kwh * p_step.ts_val;
               p_amount := p_step_portion;
               p_fit := TRUE;
            END IF;
         -- 'Camel' structure step-tariff
         ELSE
            -- when step is final step, then calculate all
            -- remaining KWHs using this step tariff value
            IF ABS (NVL (p_step.ts_interval, 0)) < min_kwh
            THEN
               p_kwh := p_remaining_kwh;
               p_step_portion := p_kwh * p_step.ts_val;
               p_amount := p_amount + p_step_portion;
               p_fit := TRUE;
            -- when step is not final, then calculate only the portion which is
            -- covered by this step. If this step covers remaining portion fully
            -- then calculate this portion and exit
            ELSE
               p_interval :=
                    p_step.ts_interval
                  * NVL (p_charge_period, 1)
                  / normalization_period;
               p_interval := NVL (p_interval, 0);

               IF (p_interval < 0)
               THEN
                  raise_application_error
                               (-20000,
                                'Distinction between two steps is not correct.'
                               );
               END IF;

               IF (p_interval >= p_remaining_kwh)
               THEN
                  p_kwh := p_remaining_kwh;
                  p_step_portion := p_kwh * p_step.ts_val;
                  p_amount := p_amount + p_step_portion;
                  p_fit := TRUE;
               ELSE
                  p_remaining_kwh := p_remaining_kwh - p_interval;
                  p_kwh := p_interval;
                  p_step_portion := p_kwh * p_step.ts_val;
                  p_amount := p_amount + p_step_portion;
               END IF;
            END IF;
         END IF;                             -- end of the "is camel?" condition

         -- Expand item details

         -- save all steps for camel tariff structure and save last
         -- step when not camel tariff
         IF     p_step_portion IS NOT NULL
            AND (p_is_camel_tariff OR (NOT p_is_camel_tariff AND p_fit))
         THEN
            p_new_detail.tariff_key := p_tar_key;
            p_new_detail.tariff_step_key := p_step.ts_key;

            IF p_negative
            THEN
               p_new_detail.gel := -p_step_portion;
               p_new_detail.vat :=
                  -tariff_manager.get_vat_charge (p_d2,
                                                  p_d2,
                                                  p_tar_key,
                                                  p_step_portion
                                                 );
               p_new_detail.kwh := -p_kwh;
            ELSE
               p_new_detail.gel := p_step_portion;
               p_new_detail.vat :=
                  tariff_manager.get_vat_charge (p_d2,
                                                 p_d2,
                                                 p_tar_key,
                                                 p_step_portion
                                                );
               p_new_detail.kwh := p_kwh;
            END IF;

            expand_item_details (p_new_detail, p_details);
         END IF;

         -- increase the index
         p_index := p_index + 1;
      END LOOP;                                         -- end of the steps loop

      -- check whether all KWHs was taken into account, if not, then raise error
      IF NOT p_fit
      THEN
         raise_application_error
                         (-20000,
                          'KWHs Charge is not fitted into given step structure'
                         );
      END IF;

      -- return calculations result
      RETURN p_amount;
   END;                                            -- end of "calc_gel" function

   /**
    * Calculate GEL.
    */
   FUNCTION calc_gel (
      pacckey       IN       NUMBER,
      pbilloper     IN       NUMBER,
      pstartdate    IN       DATE,
      penddate      IN       DATE,
      pchargedkwh   IN       NUMBER,
      pforceflat    IN       BOOLEAN,
      pdetails      IN OUT   tp_item_details,
      p_negative    IN       BOOLEAN := FALSE
   )
      RETURN NUMBER
   IS
      -- tariff cursor
      CURSOR tarcurs
      IS
         SELECT   acct.acctarkey, t.compkey, t.amount, acct.startdate,
                  acct.enddate, acct.status
             FROM acctariffs acct,
                  tarcomp t,
                  billoperation bop,
                  basetarcomp base
            WHERE acct.acckey = pacckey
              AND bop.basecompkey = base.basecompkey
              AND t.compkey = acct.compkey
              AND t.basecompkey = base.basecompkey
              AND bop.billoperkey = pbilloper
              AND (   (pstartdate BETWEEN acct.startdate AND acct.enddate)
                   OR (acct.startdate >= pstartdate AND acct.enddate <= penddate
                      )
                   OR (penddate BETWEEN acct.startdate
                                    AND NVL (acct.enddate, infinity_future)
                      )
                  )
         ORDER BY acct.startdate;

      -- calculation parameters
      days_diff        NUMBER;
      newamt           NUMBER := 0;
      chargedays       NUMBER;
      kwt_bydays       NUMBER := 0;
      amount_portion   NUMBER := 0;
   BEGIN
      -- charge days
      chargedays := penddate - pstartdate;

      -- make loop over all records in tariff cursor
      FOR r IN tarcurs
      LOOP
         -- check dates and derive charge period
         IF     r.startdate <= pstartdate
            AND penddate <= NVL (r.enddate, infinity_future)
         THEN
            days_diff := penddate - pstartdate;
         ELSIF pstartdate BETWEEN r.startdate AND r.enddate
         THEN
            days_diff := r.enddate - pstartdate;
         ELSIF r.startdate >= pstartdate AND r.enddate <= penddate
         THEN
            days_diff := r.enddate - r.startdate + 1;
         ELSIF penddate BETWEEN r.startdate AND NVL (r.enddate, infinity_future)
         THEN
            days_diff := penddate - r.startdate + 1;
         ELSE
            raise_application_error
                               (-20000,
                                'Distinction between two Dates is not correct.'
                               );
         END IF;                                 -- end of check dates condition

         -- Get charge for the given period
         IF days_diff = 0 AND chargedays = 0
         THEN
            kwt_bydays := pchargedkwh;
         ELSIF chargedays = 0
         THEN
            -- this should never happen
            raise_application_error
                      (-20000,
                          'Incorrect ''days_diff''/''chargedays'' parameters ('
                       || days_diff
                       || '/'
                       || chargedays
                       || ')'
                      );
         ELSE
            -- charge for the given period
            kwt_bydays := pchargedkwh * (days_diff / chargedays);
         END IF;                                                   -- end of the

         -- calculate GELs amount, for the period, when tariff was unchanged
         amount_portion :=
            calc_gel (r.compkey,
                      kwt_bydays,
                      days_diff,
                      pstartdate + days_diff,
                      pforceflat,
                      pdetails,
                      p_negative
                     );

         -- Assign account tariff key
         IF pdetails IS NOT NULL AND pdetails.COUNT > 0
         THEN
            -- assign only to the first detail record
            pdetails (1).acctarkey := r.acctarkey;
         END IF;

         -- increase amount
         newamt := newamt + amount_portion;
      END LOOP;

      -- return result
      RETURN newamt;
   END;                                        -- end of the 'calc_gel' function

   FUNCTION calc_gel_for_item (
      p_acc_key           IN       NUMBER,
      p_tar_acc_key       IN       NUMBER,
      p_item              IN       NUMBER,
      p_kwh               IN       NUMBER,
      p_acc_creation      IN       DATE,
      p_from_last_cycle   IN       BOOLEAN := FALSE,
      p_negative          IN       BOOLEAN := FALSE,
      p_details           IN OUT   tp_item_details
   )
      RETURN NUMBER
   IS
      d1                            DATE;
      d2                            DATE;
      p_schedule                    NUMBER;
      p_prev_cycle_last_enterdate   DATE;
      p_prev_cycle_last_item        NUMBER;
   BEGIN                            -- begin of the 'calc_gel_for_item' function
      IF p_details IS NULL
      THEN
         p_details := tp_item_details ();
      END IF;

      -- Get final date and schedule for this item
      SELECT itemdate, schedkey
        INTO d2, p_schedule
        FROM item
       WHERE itemkey = p_item;

      -- Get initial date
      IF p_from_last_cycle
      THEN
         -- look up for the previous cycle date for this account
         -- when the date can not be found, then take account creation date
         IF p_schedule IS NOT NULL                        -- for cycle reading!
         THEN
            d1 :=
               get_cycle_start_point (d2,
                                      p_acc_key,
                                      p_schedule,
                                      p_acc_creation,
                                      p_prev_cycle_last_enterdate,
                                      p_prev_cycle_last_item
                                     );
         ELSE
            BEGIN
               SELECT itemdate
                 INTO d1
                 FROM (SELECT   itemdate
                           FROM item i
                          WHERE i.acckey = p_acc_key
                            AND i.itemdate < d2
                            AND i.schedkey IS NOT NULL
                       ORDER BY itemkey DESC)
                WHERE ROWNUM = 1;
            EXCEPTION
               WHEN NO_DATA_FOUND
               THEN
                  --SELECT A.CREATEDATE INTO d1 FROM ACCOUNT A WHERE ACCKEY = p_acc_key;
                  -- @since 11/08/2006
                  d1 := p_acc_creation;
            END;
         END IF;
      ELSE
         -- look up for the previous operation date
         -- when the date can not be found, then take account creation date
         BEGIN
            SELECT itemdate
              INTO d1
              FROM (SELECT   itemdate
                        FROM item i, billoperation bop
                       WHERE i.acckey = p_acc_key
                         AND i.itemkey < p_item
                         AND bop.billoperkey = i.billoperkey
                         AND bop.opertpkey IN
                                            (oper_cat_reading, oper_cat_charge)
                         AND bop.billoperkey NOT IN
                                (oper_curr_cycle_charge_act,
                                 oper_curr_cycle_charge_voucher
                                )
                    ORDER BY itemkey DESC)
             WHERE ROWNUM = 1;
         EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
               --SELECT A.CREATEDATE INTO d1 FROM ACCOUNT A WHERE ACCKEY = p_acc_key;
               -- @since 11/08/2006
               d1 := p_acc_creation;
         END;
      END IF;

      -- Calculate GEL charge
      RETURN calc_gel (p_tar_acc_key,
                       oper_reading,
                       d1,
                       d2,
                       p_kwh,
                       FALSE,
                       p_details,
                       p_negative
                      );
   END;                               -- end of the 'calc_gel_for_item' function

   /**
    * Get tariff for the account for the given date.
    */
   FUNCTION get_tariff_for_date (
      p_account     NUMBER,
      p_some_date   DATE := SYSDATE,
      p_operation   NUMBER := oper_reading
   )
      RETURN NUMBER
   IS
      p_tar_key   NUMBER;
   BEGIN
      SELECT t.compkey
        INTO p_tar_key
        FROM acctariffs acct, tarcomp t, billoperation bop, basetarcomp base
       WHERE acct.acckey = p_account
         AND bop.basecompkey = base.basecompkey
         AND t.compkey = acct.compkey
         AND t.basecompkey = base.basecompkey
         AND bop.billoperkey = p_operation
         AND (p_some_date BETWEEN acct.startdate
                              AND NVL (acct.enddate,
                                       TO_DATE ('01012999', 'ddmmrrrr')
                                      )
             );

      RETURN p_tar_key;
   EXCEPTION
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;                             -- end of the 'get_tariff_for_date' function

   /**
    * Has step tariff function.
    */
   FUNCTION has_step_tariff (
      p_account     NUMBER,
      p_some_date   DATE := SYSDATE,
      p_operation   NUMBER := oper_reading
   )
      RETURN BOOLEAN
   IS
   BEGIN
      RETURN tariff_manager.has_step_structure
                                             (get_tariff_for_date (p_account,
                                                                   p_some_date,
                                                                   p_operation
                                                                  )
                                             );
   END;                                 -- end of the 'has_step_tariff' function

   FUNCTION has_step_tariff (
      p_account     NUMBER,
      p_d1          DATE,
      p_d2          DATE,
      p_operation   NUMBER := oper_reading
   )
      RETURN BOOLEAN
   IS
      CURSOR tarcurs
      IS
         SELECT   t.compkey
             FROM acctariffs acct,
                  tarcomp t,
                  billoperation bop,
                  basetarcomp base
            WHERE acct.acckey = p_account
              AND bop.basecompkey = base.basecompkey
              AND t.compkey = acct.compkey
              AND t.basecompkey = base.basecompkey
              AND bop.billoperkey = p_operation
              AND (   (p_d1 BETWEEN acct.startdate AND acct.enddate)
                   OR (acct.startdate >= p_d1 AND acct.enddate <= p_d2)
                   OR (p_d2 BETWEEN acct.startdate
                                AND NVL (acct.enddate, infinity_future)
                      )
                  )
         ORDER BY acct.startdate;
   BEGIN
      FOR rec IN tarcurs
      LOOP
         IF tariff_manager.has_step_structure (rec.compkey)
         THEN
            RETURN TRUE;
         END IF;
      END LOOP;

      RETURN FALSE;
   END;                                 -- end of the 'has_step_tariff' function

   /**
    * This function calcualtes operation category, from operation key.
    */
   FUNCTION get_oper_cat (p_oper_key NUMBER)
      RETURN NUMBER
   IS
      -- operation type key
      operationtypekey   NUMBER := -1;
   BEGIN                                     -- begin of "get_oper_cat" function
      -- get operation type key
      SELECT opertpkey
        INTO operationtypekey
        FROM billoperation
       WHERE billoperkey = p_oper_key;

      -- return operation type
      RETURN operationtypekey;
   -- return nothing when
   -- any exception happens
   EXCEPTION
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;                                    -- end of the 'get_oper_cat' function

   /**
    * Derive KWH charge for given period and given installed capacity.
    * This charged is calculated at day basis, using day charge (installed
    * capacity divided by days count for the previous month).
    */
   FUNCTION derive_est_kwh (p_init_date DATE, p_fin_date DATE, p_inst_cp NUMBER)
      RETURN NUMBER
   IS
      chargeperday   NUMBER := 0;
      daycount       NUMBER := bill_manager.getdays (p_init_date, p_fin_date);
   BEGIN
      -- Get day charge

      -- normalize using previous month days count
      IF normalize_on_prev_month
      THEN
         chargeperday :=
            p_inst_cp
            / EXTRACT (DAY FROM LAST_DAY (ADD_MONTHS (p_fin_date, -1)));
      -- normalize using standard 30 days period
      ELSE
         chargeperday := p_inst_cp / normalization_period;
      END IF;

      -- get full charge
      RETURN ROUND (chargeperday * daycount);
   END;                                     -- end of the 'get_est_kwh' function

   /**
    * Derives reading value for some date when previous and next reading
    * conditions are known.
    */
   FUNCTION derive_read_val (
      p_read1       NUMBER,
      p_read2       NUMBER,
      p_date1       DATE,
      p_date2       DATE,
      p_edate       DATE,
      p_mt_digits   NUMBER
   )
      RETURN NUMBER
   IS
      read1     NUMBER := p_read1;
      read2     NUMBER := p_read2;
      tempnum   NUMBER := 0;
   BEGIN
      -- Check dates
      IF p_date1 IS NULL OR p_date2 IS NULL OR p_edate IS NULL
      THEN
         raise_application_error (-20000, 'Can not accept empty dates.');
      ELSIF    TRUNC (p_date1) > TRUNC (p_edate)
            OR TRUNC (p_date2) < TRUNC (p_edate)
            OR TRUNC (p_date1) > TRUNC (p_date2)
      THEN
         raise_application_error (-20000, 'Illegal dates.');
      END IF;

      -- adjust reading values
      IF read1 > read2
      THEN
         read2 := read2 + POWER (10, p_mt_digits);
      END IF;

      -- derive reading

      -- when initial date and end dates are same,
      -- simply return second reading
      IF TRUNC (p_date1) = TRUNC (p_date2)
      THEN
         RETURN p_read2;
      END IF;

      tempnum :=
           read1
         +   (read2 - read1)
           * (  (TRUNC (p_edate) - TRUNC (p_date1))
              / (TRUNC (p_date2) - TRUNC (p_date1))
             );
      RETURN tempnum;
   END;                                 -- end of the 'derive_read_val' function

   /**
    * Derive read value when initial reading and futher charge is known.
    */
   FUNCTION derive_read_val (
      p_iread         NUMBER,
      p_charged_kwh   NUMBER,
      p_coeff         NUMBER,
      p_digits        NUMBER
   )
      RETURN NUMBER
   IS
      coeff     NUMBER := p_coeff;
      newread   NUMBER := 0;
   BEGIN
      -- adjust coefficient
      IF p_coeff < 1
      THEN
         coeff := 1;
      END IF;

      -- calculate new reading
      newread := p_iread + p_charged_kwh / coeff;
      -- return reading
      RETURN newread;
   END;                                 -- end of the 'derive_read_val' function

   /**
    * Derive KWHs charge between two readings and one control reading.
    */
   FUNCTION derive_kwh (
      curr_r          NUMBER,
      prev_r          NUMBER,
      cont_r          NUMBER,
      coeff           NUMBER,
      digit           NUMBER,
      p_force_exact   BOOLEAN := FALSE
   )
      RETURN NUMBER
   IS
      tempr1     NUMBER := curr_r;
      tempr2     NUMBER := prev_r;
      mttcoeff   NUMBER := coeff;
   BEGIN
      -- adjust metter coefficient
      IF (coeff IS NULL OR coeff < 1)
      THEN
         mttcoeff := 1;
      END IF;

      -- adjust current reading
      IF (curr_r < cont_r)
      THEN
         tempr1 := POWER (10, digit) + curr_r;
      END IF;

      -- adjust previous reading
      IF (prev_r < cont_r)
      THEN
         tempr2 := POWER (10, digit) + prev_r;
      END IF;

      -- Calculate KWH charge
      IF p_force_exact
      THEN
         RETURN (tempr1 - tempr2) * mttcoeff;
      ELSE
         RETURN ROUND (tempr1 * mttcoeff) - ROUND (tempr2 * mttcoeff);
      END IF;
   END;                                      -- end of the 'derive_kwh' function

   /**
    * Get cycle key.
    */
   FUNCTION get_cycle (p_sched_key NUMBER)
      RETURN NUMBER
   IS
      p_cycle   NUMBER;
   BEGIN
      -- get last cycle
      IF p_sched_key IS NOT NULL
      THEN
         SELECT cyclekey
           INTO p_cycle
           FROM schedule
          WHERE schedkey = p_sched_key;
      END IF;

      -- return results
      RETURN p_cycle;
   -- exceptions handling
   EXCEPTION
      -- return NONE cycle when any
      -- exception happens
      WHEN OTHERS
      THEN
         RETURN cycle_none;
   END;                                       -- end of the 'get_cycle' function

   /**
    * Returns first not-cycle reading for this cycle. Is schedule = NULL, then
    * current cycle is considered.
    */
   FUNCTION get_1st_not_cycle_r_for_cycle (p_account NUMBER, p_schedule NUMBER)
      RETURN item%ROWTYPE
   IS
      p_prev_cycle_last_item        NUMBER;
      p_prev_cycle_last_itemdate    DATE;
      p_prev_cycle_last_enterdate   DATE;
      p_item                        item%ROWTYPE;
      p_date                        DATE;
   BEGIN
      -- get UPPER BOUND date
      IF p_schedule IS NULL
      THEN
         p_date := TRUNC (SYSDATE);
      ELSE
         p_date := get_cycle_final_date (p_schedule);
      END IF;

      -- get previous cycle information
      get_previous_cycle_info (p_account,
                               p_schedule,
                               p_prev_cycle_last_item,
                               p_prev_cycle_last_itemdate,
                               p_prev_cycle_last_enterdate
                              );

      BEGIN
         -- get item info
         SELECT *
           INTO p_item
           FROM (SELECT   i.*
                     FROM item i, billoperation bop
                    WHERE i.billoperkey = bop.billoperkey
                      AND bop.opertpkey = oper_cat_reading
                      AND i.acckey = p_account
                      AND schedkey IS NULL
                      AND itemdate <= p_date
                      AND itemkey > p_prev_cycle_last_item
                      AND
                          -- emprove performance
                          enterdate > p_prev_cycle_last_enterdate
                 ORDER BY i.itemkey)
          WHERE ROWNUM = 1;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            NULL;
      END;

      -- return results
      RETURN p_item;
   END;                  -- end of the 'get_1st_not_cycle_R_for_cycle' procedure

   /**
    * Returns information about previous cycle.
    */
   PROCEDURE get_previous_cycle_info (
      p_account                           NUMBER,
      p_schedule                          NUMBER,
      p_prev_cycle_last_item        OUT   NUMBER,
      p_prev_cycle_last_itemdate    OUT   DATE,
      p_prev_cycle_last_enterdate   OUT   DATE
   )
   IS
   BEGIN
      BEGIN
         SELECT itemkey, enterdate,
                itemdate
           INTO p_prev_cycle_last_item, p_prev_cycle_last_enterdate,
                p_prev_cycle_last_itemdate
           FROM (SELECT   itemkey, enterdate, itemdate
                     FROM item
                    WHERE acckey = p_account
                      AND schedkey IS NOT NULL
                      AND schedkey < NVL (p_schedule, 1000000000)
                      AND
                          -- @since 11/07/2006 cut on ENTERDATE to improve performance
                          enterdate > item_cut_date
                 ORDER BY itemkey DESC)
          WHERE ROWNUM = 1;
      EXCEPTION
         WHEN NO_DATA_FOUND
         THEN
            BEGIN
               SELECT itemkey, enterdate,
                      itemdate
                 INTO p_prev_cycle_last_item, p_prev_cycle_last_enterdate,
                      p_prev_cycle_last_itemdate
                 FROM (SELECT   itemkey, enterdate, itemdate
                           FROM item
                          WHERE acckey = p_account
                            AND schedkey IS NOT NULL
                            AND schedkey < NVL (p_schedule, 1000000000)
                       ORDER BY itemkey DESC)
                WHERE ROWNUM = 1;
            EXCEPTION
               WHEN NO_DATA_FOUND
               THEN
                  NULL;
            END;
      END;

      -- Adjust when not found
      IF p_prev_cycle_last_item IS NULL
      THEN
         p_prev_cycle_last_item := -1;
         p_prev_cycle_last_enterdate := infinity_past;
         p_prev_cycle_last_itemdate := infinity_past;
      END IF;
   END;                        -- end of the 'get_previous_cycle_info' procedure

   /**
    * Get schedule.
    */
   FUNCTION get_schedule (p_acc_key NUMBER, p_cycle_key NUMBER)
      RETURN NUMBER
   IS
      p_schedule   NUMBER;
   BEGIN
      -- Get schedule
      SELECT s.schedkey
        INTO p_schedule
        FROM schedule s, item i
       WHERE i.schedkey = s.schedkey
         AND i.acckey = p_acc_key
         AND s.cyclekey = p_cycle_key
         AND ROWNUM = 1;                                     -- force ONE record

      -- return schedule
      RETURN p_schedule;
   -- exceptions handling
   EXCEPTION
      -- return nothing when any
      -- exception happens
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;                                    -- end of the 'get_schedule' function

   /**
    * Get last read for the account. When <tt>p_real_read</tt> parameter is
    * <tt>True</tt>, then this function returns last real read.
    */
   FUNCTION get_last_read (
      p_acc_key     NUMBER,
      p_max_date    DATE,
      p_real_read   BOOLEAN := FALSE
   )
      RETURN item%ROWTYPE
   IS
      p_item   item%ROWTYPE;
   BEGIN
      -- when maximal date is not defined, then
      -- select actualy last operation for this account ...
      IF p_max_date IS NULL
      THEN
         -- get real reading
         IF p_real_read
         THEN
            SELECT i.*
              INTO p_item
              FROM item i
             WHERE i.itemkey =
                      (SELECT MAX (i2.itemkey)
                         FROM item i2, billoperation bop
                        WHERE i2.billoperkey = bop.billoperkey
                          AND bop.opertpkey IN (1)
                          AND i2.acckey = p_acc_key
                          AND i2.billoperkey NOT IN
                                 (oper_curr_cycle_charge,
                                  oper_curr_cycle_charge_act,
                                  oper_curr_cycle_charge_voucher,
                                  oper_summary,
                                  oper_curr_cycle_gel_discharge
                                 ));
         -- get last reading
         ELSE
            SELECT i.*
              INTO p_item
              FROM item i
             WHERE i.itemkey =
                      (SELECT MAX (i2.itemkey)
                         FROM item i2, billoperation bop
                        WHERE i2.billoperkey = bop.billoperkey
                          AND bop.opertpkey IN (1, 2)
                          AND i2.acckey = p_acc_key
                          AND i2.billoperkey NOT IN
                                 (oper_curr_cycle_charge,
                                  oper_curr_cycle_charge_act,
                                  oper_curr_cycle_charge_voucher,
                                  oper_summary,
                                  oper_curr_cycle_gel_discharge
                                 ));
         END IF;                                -- end of real reading condition
      -- ... else, select last item for this account for
      -- given maximal date
      ELSE
         -- get real reading
         IF p_real_read
         THEN
            SELECT i.*
              INTO p_item
              FROM item i
             WHERE i.itemkey =
                      (SELECT MAX (i2.itemkey)
                         FROM item i2, billoperation bop
                        WHERE i2.billoperkey = bop.billoperkey
                          AND bop.opertpkey IN (1)
                          AND i2.acckey = p_acc_key
                          AND i2.itemdate <= p_max_date
                          AND i2.billoperkey NOT IN
                                 (oper_curr_cycle_charge,
                                  oper_curr_cycle_charge_act,
                                  oper_curr_cycle_charge_voucher,
                                  oper_summary,
                                  oper_curr_cycle_gel_discharge
                                 ));
         -- get last reading
         ELSE
            SELECT i.*
              INTO p_item
              FROM item i
             WHERE i.itemkey =
                      (SELECT MAX (i2.itemkey)
                         FROM item i2, billoperation bop
                        WHERE i2.billoperkey = bop.billoperkey
                          AND bop.opertpkey IN (1, 2)
                          AND i2.acckey = p_acc_key
                          AND i2.itemdate <= p_max_date
                          AND i2.billoperkey NOT IN
                                 (oper_curr_cycle_charge,
                                  oper_curr_cycle_charge_act,
                                  oper_curr_cycle_charge_voucher,
                                  oper_summary,
                                  oper_curr_cycle_gel_discharge
                                 ));
         END IF;                                -- end of real reading condition
      END IF;                                   -- end of maximal date condition

      -- return results
      RETURN p_item;
   -- when any exception happens return nothing
   EXCEPTION
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;                                   -- end of the 'get_last_read' function

   /**
    * Get step-tariff by given tariff key.
    */
   FUNCTION get_tar_comp_step (p_tar_key IN NUMBER)
      RETURN tp_tar_steps
   IS
      p_steps   tp_tar_steps;
      p_step    tariff_step%ROWTYPE;
      p_index   NUMBER;
      p_size    NUMBER;
   BEGIN                           -- begin of the 'get_tar_comp_step' procedure
      -- Try to find records in cache
      IF tariff_cache IS NOT NULL AND tariff_cache.COUNT > 0
      THEN
         p_index := 1;

         WHILE p_index <= tariff_cache.COUNT
         LOOP
            p_steps := tariff_cache (p_index);

            IF p_steps IS NOT NULL AND p_steps.COUNT > 0
            THEN
               p_step := p_steps (1);

               IF p_step.ts_tar_key = p_tar_key
               THEN
                  RETURN p_steps;
               END IF;
            END IF;

            p_index := p_index + 1;
         END LOOP;
      END IF;                                                      -- end of the

      -- reset counter
      p_index := 1;

      -- Iterate over all steps which meet the criteria. Steps are ordered by
      -- the sequence parameter
      FOR step_rec IN (SELECT   ts.*
                           FROM tariff_step ts
                          WHERE ts.ts_tar_key = p_tar_key
                       ORDER BY ts.ts_seq)
      LOOP
         -- initialize steps table
         IF p_steps IS NULL
         THEN
            p_steps := tp_tar_steps ();
         END IF;

         -- add a new step to the table
         p_steps.EXTEND;
         p_steps (p_index) := step_rec;
         -- increase index
         p_index := p_index + 1;
      END LOOP;                                             -- end of steps loop

      -- Add to cache
      IF p_steps IS NOT NULL AND p_steps.COUNT > 0
      THEN
         -- initialize cache
         IF tariff_cache IS NULL
         THEN
            tariff_cache := tp_steps_cache ();
         END IF;

         -- determine index and add
         p_size := tariff_cache.COUNT;
         tariff_cache.EXTEND;
         tariff_cache (p_size + 1) := p_steps;
      END IF;

      -- return results
      RETURN p_steps;
   END;                                    -- end of the 'get_step_tar' function

   /**
    * Get flat tariff by given tariff key.
    */
   FUNCTION get_tar_comp_flat (p_tar_key IN NUMBER)
      RETURN tarcomp%ROWTYPE
   IS
      p_flat_tar   tarcomp%ROWTYPE;
   BEGIN
      -- selecting data into the row
      SELECT tc.*
        INTO p_flat_tar
        FROM tarcomp tc
       WHERE tc.compkey = p_tar_key;

      -- return the result
      RETURN p_flat_tar;
   END;                                    -- end of the 'get_flat_tar' function

   /**
    * Returns account parent for the given one.
    */
   FUNCTION get_parent_account (p_account NUMBER)
      RETURN tp_parent
   IS
      -- prepare parent
      PARENT   tp_parent;
   BEGIN
      -- select data
      SELECT accrel.base_acckey,
             par.custkey
        INTO PARENT
        FROM accrel,
             ACCOUNT par,
             customer par_cust,
             ACCOUNT acc,
             customer cust,
             address adr
       WHERE accrel.acckey = p_account
         AND accrel.base_acckey = par.acckey
         AND par.custkey = par_cust.custkey
         AND accrel.acckey = acc.acckey
         AND acc.custkey = cust.custkey
         AND cust.premisekey = adr.premisekey
         AND accrel.reltype IN (4, 8)
         AND (par_cust.custcatkey NOT IN (5, 6));

      -- return parent record
      RETURN PARENT;
   EXCEPTION
      -- return nothing when any exception happens
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;                              -- end of the 'get_parent_account' function

   /**
    * Clears all warnings in error messages table.
    */
   PROCEDURE clear_warnings
   IS
   BEGIN
      -- clear error messages table
      EXECUTE IMMEDIATE ' truncate table  error_messages_t';
   END;                                      -- end of 'clear_warnings' function

   /**
    * Submits error to the errors table.
    */
   PROCEDURE submit_error (err_code NUMBER, err_text VARCHAR2)
   IS
   BEGIN
      -- call old BILL_MANAGER's error function
      bill_manager.errorproc (err_code, SUBSTR (err_text, 1, 200));
   END;                                   -- end of the 'submit_error' procedure

   /**
    * Submits error to the errors table.
    */
   PROCEDURE submit_warning (err_code NUMBER, err_text VARCHAR2)
   IS
   BEGIN
      -- call old BILL_MANAGER's error function
      bill_manager.errorproc (err_code, err_text, 1);
   END;                                 -- end of the 'submit_warning' procedure

   /**
    * Get account information.
    */
   PROCEDURE get_acc_info (
      p_acc_key        IN       NUMBER,
      p_acc_cr_date    OUT      DATE,
      p_inst_cp        OUT      NUMBER,
      p_is_cutted      OUT      BOOLEAN,
      p_cust_key       OUT      NUMBER,
      p_cust_cat_key   OUT      NUMBER,
      p_mtt_type       OUT      NUMBER,
      p_mtt_digits     OUT      NUMBER,
      p_mtt_coeff      OUT      NUMBER,
      p_min_kwh        OUT      NUMBER,
      p_max_kwh        OUT      NUMBER,
      p_balance        OUT      NUMBER,
      p_is_main_acc    OUT      BOOLEAN,
      p_is_closed      OUT      BOOLEAN
   )
   IS
      p_cut_as_num   NUMBER;
      p_main_acc     NUMBER;
      p_status       NUMBER;
   BEGIN
      -- select account information
      SELECT a.custkey, c.custcatkey, a.inst_cp, a.mttpkey, m.digit,
             a.mtkoef, a.createdate, a.cut, c.balance, a.mainaccount,
             a.statuskey
        INTO p_cust_key, p_cust_cat_key, p_inst_cp, p_mtt_type, p_mtt_digits,
             p_mtt_coeff, p_acc_cr_date, p_cut_as_num, p_balance, p_main_acc,
             p_status
        FROM ACCOUNT a, customer c, mttype m
       WHERE a.acckey = p_acc_key
         AND a.custkey = c.custkey
         AND a.mttpkey = m.mttpkey;

      -- convert customer cut status indicating value
      -- to the boolean flag
      p_is_cutted := (p_cut_as_num IS NOT NULL AND p_cut_as_num = 1);
      -- convert is main account parameter
      p_is_main_acc := p_main_acc IS NOT NULL AND p_main_acc = 1;
      -- is account closed?
      p_is_closed := p_status IS NULL OR p_status = 2;
   END;                                   -- end of the 'get_acc_info' procedure

   /**
    * Get array of accounts, which belongs to the given customer.
    */
   FUNCTION get_accounts (p_cust_key NUMBER, p_active BOOLEAN := FALSE)
      RETURN num_array
   IS
      -- temporary parameters
      p_accounts   num_array;
      p_index      NUMBER    := 1;
   BEGIN
      -- Check input

      -- return nothing when customer key is not defined
      IF p_cust_key IS NULL
      THEN
         RETURN NULL;
      END IF;

      -- Loop over all acounts
      FOR acc_rec IN (SELECT *
                        FROM ACCOUNT
                       WHERE custkey = p_cust_key)
      LOOP
         IF NOT (p_active AND acc_rec.statuskey = 2)
         THEN
            -- initialize accounts array when neccessary
            IF p_accounts IS NULL
            THEN
               p_accounts := num_array ();
            END IF;

            -- Add a new record
            p_accounts.EXTEND;
            p_accounts (p_index) := acc_rec.acckey;
            -- Extend index
            p_index := p_index + 1;
         END IF;
      END LOOP;                                               -- end of the loop

      -- Return results
      RETURN p_accounts;
   -- Exceptions handling
   EXCEPTION
      -- return nothing when any exception happens
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;                                    -- end of the 'get_accounts' function

   /**
    * Get job for the operator.
    */
   FUNCTION get_job (p_oper_key NUMBER)
      RETURN NUMBER
   IS
      p_job_key   NUMBER;
   BEGIN
      -- get job key
      SELECT jobkey
        INTO p_job_key
        FROM person
       WHERE perskey = p_oper_key;

      -- return results
      RETURN p_job_key;
   -- return NULL when exception happens
   EXCEPTION
      WHEN OTHERS
      THEN
         RETURN NULL;
   END;                                             -- end of 'get_job' function

   /**
    * Returns correction operation code for the given year.
    */
   FUNCTION get_correction_code (p_year NUMBER, p_type NUMBER)
      RETURN NUMBER
   IS
   BEGIN
      -- 2005
      IF p_year = 2005
      THEN
         IF p_type = discharge_plain
         THEN
            RETURN 292;
         ELSIF p_type = discharge_psubs
         THEN
            RETURN 308;
         ELSIF p_type = discharge_parent
         THEN
            RETURN 197;
         ELSIF p_type = recharge_plain
         THEN
            RETURN 197;
         ELSIF p_type = recharge_psubs
         THEN
            RETURN 223;
         ELSIF p_type = recharge_parent
         THEN
            RETURN 292;
         END IF;
      -- 2006
      ELSIF p_year = 2006
      THEN
         IF p_type = discharge_plain
         THEN
            RETURN 291;
         ELSIF p_type = discharge_psubs
         THEN
            RETURN 309;
         ELSIF p_type = discharge_parent
         THEN
            RETURN 199;
         ELSIF p_type = recharge_plain
         THEN
            RETURN 199;
         ELSIF p_type = recharge_psubs
         THEN
            RETURN 224;
         ELSIF p_type = recharge_parent
         THEN
            RETURN 291;
         END IF;
      -- 2007
      ELSIF p_year = 2007
      THEN
         IF p_type = discharge_plain
         THEN
            RETURN 351;
         ELSIF p_type = discharge_psubs
         THEN
            RETURN 358;
         ELSIF p_type = discharge_parent
         THEN
            RETURN 352;
         ELSIF p_type = recharge_plain
         THEN
            RETURN 352;
         ELSIF p_type = recharge_psubs
         THEN
            RETURN 359;
         ELSIF p_type = recharge_parent
         THEN
            RETURN 351;
         END IF;
      -- 2008
      ELSIF p_year = 2008
      THEN
         IF p_type = discharge_plain
         THEN
            RETURN 531;
         ELSIF p_type = discharge_psubs
         THEN
            RETURN 538;
         ELSIF p_type = discharge_parent
         THEN
            RETURN 532;
         ELSIF p_type = recharge_plain
         THEN
            RETURN 532;
         ELSIF p_type = recharge_psubs
         THEN
            RETURN 539;
         ELSIF p_type = recharge_parent
         THEN
            RETURN 531;
         END IF;
      -- 2009
      ELSIF p_year = 2009
      THEN
        IF p_type = discharge_plain
         THEN
            RETURN 551;
         ELSIF p_type = discharge_psubs
         THEN
            RETURN 558;
         ELSIF p_type = discharge_parent
         THEN
            RETURN 552;
         ELSIF p_type = recharge_plain
         THEN
            RETURN 552;
         ELSIF p_type = recharge_psubs
         THEN
            RETURN 559;
         ELSIF p_type = recharge_parent
         THEN
            RETURN 551;
         END IF;
      -- 2010
      ELSIF p_year = 2010
      THEN
        IF p_type = discharge_plain
         THEN
            RETURN 561;
         ELSIF p_type = discharge_psubs
         THEN
            RETURN 568;
         ELSIF p_type = discharge_parent
         THEN
            RETURN 562;
         ELSIF p_type = recharge_plain
         THEN
            RETURN 562;
         ELSIF p_type = recharge_psubs
         THEN
            RETURN 569;
         ELSIF p_type = recharge_parent
         THEN
            RETURN 561;
         END IF;
      -- 2011
      ELSIF p_year = 2011
      THEN
        IF p_type = discharge_plain
         THEN
            RETURN 571;
         ELSIF p_type = discharge_psubs
         THEN
            RETURN 578;
         ELSIF p_type = discharge_parent
         THEN
            RETURN 572;
         ELSIF p_type = recharge_plain
         THEN
            RETURN 572;
         ELSIF p_type = recharge_psubs
         THEN
            RETURN 579;
         ELSIF p_type = recharge_parent
         THEN
            RETURN 571;
         END IF;
      END IF;

      -- Unsupported operation
      raise_application_error (-20000,
                                  'Unsupported operation type ('
                               || p_type
                               || ') or year ('
                               || p_year
                               || ').'
                              );
   END;                             -- end of the 'get_correction_code' function

   FUNCTION get_cycle_start_point (
      p_end_point                   IN       DATE,
      p_account                     IN       NUMBER,
      p_schedule                    IN       NUMBER,
      p_acc_creation                IN       DATE,
      p_prev_cycle_last_enterdate   OUT      DATE,
      p_prev_cycle_last_item        OUT      NUMBER
   )
      RETURN DATE
   IS
      p_prev_cycle_is_too_far      BOOLEAN;
      p_prev_cycle_last_itemdate   DATE;
      p_first_not_cycle_r          item%ROWTYPE;
      d1                           DATE;
   BEGIN
      get_previous_cycle_info (p_account,
                               p_schedule,
                               p_prev_cycle_last_item,
                               p_prev_cycle_last_itemdate,
                               p_prev_cycle_last_enterdate
                              );
      p_prev_cycle_is_too_far :=
         ABS (TRUNC (p_end_point) - TRUNC (p_prev_cycle_last_itemdate)) >
                                                  previous_cycle_max_distinction;

      IF p_prev_cycle_is_too_far
      THEN
         p_first_not_cycle_r :=
                          get_1st_not_cycle_r_for_cycle (p_account, p_schedule);

         -- no reading before!
         IF p_first_not_cycle_r.itemkey IS NULL
         THEN
            IF ABS (p_acc_creation - p_end_point) >
                                                 previous_cycle_max_distinction
            THEN
               d1 := TRUNC (p_end_point - previous_cycle_max_distinction);
            ELSE
               d1 := TRUNC (p_acc_creation);
            END IF;
         ELSE
            d1 := TRUNC (p_first_not_cycle_r.itemdate);
         END IF;
      -- when previous cycle is not so far
      ELSE
         d1 := TRUNC (p_prev_cycle_last_itemdate);
      END IF;

      RETURN d1;
   END;                           -- end of the 'get_cycle_start_point' function

   FUNCTION get_cycle_initial_date (p_account NUMBER, p_schedule NUMBER)
      RETURN DATE
   IS
      d1   DATE;
   BEGIN
      SELECT MAX (itemdate)
        INTO d1
        FROM item
       WHERE acckey = p_account
         AND schedkey IS NOT NULL
         AND schedkey < p_schedule
         AND enterdate > item_cut_date;

      IF d1 IS NULL
      THEN
         SELECT MAX (itemdate)
           INTO d1
           FROM item
          WHERE acckey = p_account
            AND schedkey IS NOT NULL
            AND schedkey < p_schedule;
      END IF;

      RETURN d1;
   END;                          -- end of the 'get_cycle_initial_date' function

   FUNCTION get_cycle_final_date (p_schedule NUMBER)
      RETURN DATE
   IS
      d2   DATE;
   BEGIN
      SELECT s.cycledate
        INTO d2
        FROM schedule s
       WHERE s.schedkey = p_schedule;

      RETURN d2;
   END;                            -- end of the 'get_cycle_final_date' function

   PROCEDURE expand_item_details (
      new_details   IN       tp_item_details,
      all_details   IN OUT   tp_item_details
   )
   IS
      p_index      NUMBER         := 1;
      new_detail   tp_item_detail;
   BEGIN
      -- New details are empty
      IF new_details IS NULL OR new_details.COUNT = 0
      THEN
         RETURN;
      END IF;

      -- All details checking
      IF all_details IS NULL
      THEN
         all_details := tp_item_details ();
      END IF;

      -- Expand all details, step by step
      WHILE p_index <= new_details.COUNT
      LOOP
         new_detail := new_details (p_index);
         expand_item_details (new_detail, all_details);
         p_index := p_index + 1;
      END LOOP;
   END;                            -- end of the 'expand_item_details' procedure

   /**
    * Procedure for extending item details using single new detail.
    */
   PROCEDURE expand_item_details (
      new_detail    IN       tp_item_detail,
      all_details   IN OUT   tp_item_details
   )
   IS
      tar_key    NUMBER         := new_detail.tariff_key;
      step_key   NUMBER         := new_detail.tariff_step_key;
      gel        NUMBER         := new_detail.gel;
      vat        NUMBER         := new_detail.vat;
      a_detail   tp_item_detail;
      p_index    NUMBER         := 1;
      p_size     NUMBER;
   BEGIN
      -- when tariff key is not defined
      IF tar_key IS NULL
      THEN
         RETURN;
      END IF;

      -- initialize all details, if not yet initialized
      IF all_details IS NULL
      THEN
         all_details := tp_item_details ();
      END IF;

      -- loop over all details
      WHILE p_index <= all_details.COUNT
      LOOP
         -- get detail value
         a_detail := all_details (p_index);

         IF     tar_key = a_detail.tariff_key
            AND (   (step_key IS NULL AND a_detail.tariff_step_key IS NULL)
                 OR (step_key IS NOT NULL
                     AND step_key = a_detail.tariff_step_key
                    )
                )
         THEN
            a_detail.gel := NVL (a_detail.gel, 0) + NVL (gel, 0);
            a_detail.vat := NVL (a_detail.vat, 0) + NVL (vat, 0);
            RETURN;
         END IF;

         -- increase index
         p_index := p_index + 1;
      END LOOP;                                      -- end of the details loops

      -- add a new detail
      p_size := all_details.COUNT;
      all_details.EXTEND;
      all_details (p_size + 1) := new_detail;
   END;                            -- end of the 'expand_item_details' procedure
END;                                        -- end of the package specification;
/

