/* measure 1 - hospital admissions with a primary diagnosis of drug-related mental and behavioural disorders */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 LIKE 'F1%'
                AND diag.DiagCode4 NOT LIKE 'F10%'
                AND diag.DiagCode4 NOT LIKE 'F17%'
		AND diag.DiagIdx = 1)
GROUP BY ADMIDATE

/* measure 2 - hospital admissions with a primary diagnosis of poisoning by drugs */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 LIKE 'T40%'
		OR diag.DiagCode4 = 'T436'
		AND diag.DiagIdx = 1)
GROUP BY ADMIDATE
