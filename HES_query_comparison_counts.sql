/* staph: all */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 IN ('B956', 'B957', 'B958'))
GROUP BY ADMIDATE

/* staph: skin */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 IN ('B956', 'B957', 'B958'))
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 LIKE 'L%'
		AND diag.Diagidx = 1)
GROUP BY ADMIDATE

/* strep: all */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 IN ('B950', 'B951', 'B952', 'B953', 'B954', 'B955'))
GROUP BY ADMIDATE

/* strep: skin */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 IN ('B950', 'B951', 'B952', 'B953', 'B954', 'B955'))
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 LIKE 'L%'
		AND diag.Diagidx = 1)
GROUP BY ADMIDATE

/* F11 */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vHES_APC_Flat]
WHERE DIAG3_01 = 'F11'
AND EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
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
