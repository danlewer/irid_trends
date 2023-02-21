-- staph and strep sin infections; not opioid related

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

-- measure 1 - hospital admissions with a primary diagnosis of drug-related mental and behavioural disorders

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

-- measure 2 - hospital admissions with a primary diagnosis of poisoning by drugs

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

-- F11 only

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vtHES_APC]
WHERE EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
AND EPIKEY IN (SELECT diag.EPIKEY
		FROM HES_APC.dbo.[vtHES_APC_DIAG] AS diag
		WHERE diag.DiagCode4 LIKE 'F11%'
		AND diag.DiagIdx = 1)
GROUP BY ADMIDATE
