SELECT main.EPIKEY, main.FYEAR, main.TOKEN_PERSON_ID, diag.DiagCode4, main.CLASSPAT, main.EPISTAT, main.EPIORDER, main.STARTAGE, main.SEX, main.ETHNOS, main.ADMIDATE, main.ADMIMETH, main.DISDATE, main.DISMETH, main.RESGOR, main.RESGOR_ONS, main.RESLADST, main.IMD04RK, main.LSOA11
FROM HES_APC.dbo.[vtHES_APC] AS main
INNER JOIN HES_APC.dbo.[vtHES_APC_DIAG] AS diag
ON main.EPIKEY = diag.EPIKEY
WHERE (diag.DiagCode4 LIKE 'L02%'    /* Abscess */
    OR diag.DiagCode4 LIKE 'L03%' /* Cellulitis */
	OR diag.DiagCode4 LIKE 'I80%' /* Phlebitis & thrombophlebitis */
	OR diag.DiagCode4 = 'A480' /* Other SSTI */
	OR diag.DiagCode4 = 'L088'
	OR diag.DiagCode4 = 'L089'
	OR diag.DiagCode4 LIKE 'L97%'
	OR diag.DiagCode4 = 'L984'
	OR diag.DiagCode4 = 'L988'
	OR diag.DiagCode4 = 'L989'
	OR diag.DiagCode4 LIKE 'R02%'
	OR diag.DiagCode4 = 'B376' /* Endocarditis */
	OR diag.DiagCode4 = 'I330'
	OR diag.DiagCode4 = 'I339'
	OR diag.DiagCode4 LIKE 'I38%'
	OR diag.DiagCode4 LIKE 'I39%'
	OR diag.DiagCode4 LIKE 'A40%' /* Septicaemia */
	OR diag.DiagCode4 LIKE 'A41%'
	OR diag.DiagCode4 = 'R572'
	OR diag.DiagCode4 = 'B377'
	OR diag.DiagCode4 LIKE 'M86%' /* Osteomyelitis & septic arthritis */
	OR diag.DiagCode4 LIKE 'M00%'
	OR diag.DiagCode4 = 'M465'
	OR diag.DiagCode4 = 'M762' /* Necrotising fasciitis */
	)
AND diag.DiagIdx = 1
AND main.EPIKEY IN (SELECT diag1.EPIKEY
					FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag1
					WHERE diag1.DiagCode4 LIKE 'F11%'
					AND diag1.DiagIdx BETWEEN 2 AND 20)
