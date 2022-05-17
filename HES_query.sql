SELECT EPIKEY, TOKEN_PERSON_ID, PSEUDO_HESID, ENCRYPTED_HESID, EPIORDER, EPISTAT, FYEAR, ADMIDATE, DISDATE, SEX, ETHNOS, IMD04RK, STARTAGE, SPELDUR, RESLADST, RESGOR, ADMIMETH, DISMETH, DISDEST, DIAG4_01

/* secondary diagnoses - not extracted */
/* , DIAG4_02, DIAG4_03, DIAG4_04, DIAG4_05, DIAG4_06, DIAG4_07, DIAG4_08, DIAG4_09, DIAG4_10, DIAG4_11, DIAG4_12, DIAG4_13, DIAG4_14, DIAG4_15, DIAG4_16, DIAG4_17, DIAG4_18, DIAG4_19, DIAG4_20 */

FROM HES_APC.dbo.[vHES_APC_Flat]

WHERE (DIAG3_01 = 'L02'    /* Abscess */
    OR DIAG3_01 = 'L03' /* Cellulitis */
    OR DIAG3_01 = 'I80' /* Phlebitis & thrombophlebitis */
    OR DIAG4_01 = 'A480' /* Other SSTI */
    OR DIAG4_01 = 'L088'
    OR DIAG4_01 = 'L089'
    OR DIAG3_01 = 'L97'
    OR DIAG4_01 = 'L984'
    OR DIAG4_01 = 'L988'
    OR DIAG4_01 = 'L989'
    OR DIAG3_01 = 'R02'
    OR DIAG4_01 = 'B376' /* Endocarditis */
    OR DIAG4_01 = 'I330'
    OR DIAG4_01 = 'I339'
    OR DIAG3_01 = 'I38'
    OR DIAG3_01 = 'I39'
    OR DIAG3_01 = 'A40' /* Septicaemia */
    OR DIAG3_01 = 'A41'
    OR DIAG4_01 = 'R572'
    OR DIAG4_01 = 'B377'
    OR DIAG3_01 = 'M86' /* Osteomyelitis & septic arthritis */
    OR DIAG3_01 = 'M00'
    OR DIAG4_01 = 'M465'
    OR DIAG4_01 = 'M762') /* Necrotising fasciitis */

AND   (DIAG3_02 = 'F11'
    OR DIAG3_03 = 'F11'
    OR DIAG3_04 = 'F11'
    OR DIAG3_05 = 'F11'
    OR DIAG3_06 = 'F11'
    OR DIAG3_07 = 'F11'
    OR DIAG3_08 = 'F11'
    OR DIAG3_09 = 'F11'
    OR DIAG3_10 = 'F11'
    OR DIAG3_11 = 'F11'
    OR DIAG3_12 = 'F11'
    OR DIAG3_13 = 'F11'
    OR DIAG3_14 = 'F11'
    OR DIAG3_15 = 'F11'
    OR DIAG3_16 = 'F11'
    OR DIAG3_17 = 'F11'
    OR DIAG3_18 = 'F11'
    OR DIAG3_19 = 'F11'
    OR DIAG3_20 = 'F11')
