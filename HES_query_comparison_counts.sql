/* staph and strep sin infections; not opioid related */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag1.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag1
		WHERE diag1.DiagCode4 LIKE 'B95%'
		AND diag1.DiagCode4 NOT LIKE 'F11%')
AND EPIKEY IN (SELECT diag2.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag2
		WHERE diag2.DiagCode4 LIKE 'L%'
		AND diag2.Diagidx = 1)
GROUP BY ADMIDATE

/* T-codes */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 IN ('T400', 'T401', 'T402', 'T403', 'T404')
		AND diag.DiagIdx = 1)
GROUP BY ADMIDATE

/* Other overdoses */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 IN ('T400', 'T401', 'T402', 'T403', 'T404'))
AND EPIKEY IN (SELECT diag1.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag1
		WHERE diag1.DiagCode4 LIKE 'F11%'
		AND diag1.DiagIdx = 1)
GROUP BY ADMIDATE
