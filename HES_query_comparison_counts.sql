/* drugs */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vHES_APC_Flat]
WHERE DIAG3_01 IN ('F11', 'F12', 'F13', 'F14', 'F15', 'F16', 'F18', 'F19')
AND EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
GROUP BY ADMIDATE

/* respiratory infections */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vHES_APC_Flat]
WHERE DIAG3_01 IN ('J00', 'J01', 'J02', 'J03', 'J04', 'J05', 'J06', 'J07', 'J08', 'J09', 'J10', 'J11', 'J12', 'J13', 'J14', 'J15', 'J16', 'J17', 'J18', 'J19', 'J20', 'J21', 'J22')
AND EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
GROUP BY ADMIDATE

/* intestinal infections */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vHES_APC_Flat]
WHERE DIAG3_01 IN ('A00', 'A01', 'A02', 'A03', 'A04', 'A05', 'A06', 'A07', 'A08', 'A09')
AND EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
GROUP BY ADMIDATE

/* tb */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vHES_APC_Flat]
WHERE DIAG3_01 IN ('A15', 'A16', 'A17', 'A18', 'A19')
AND EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
GROUP BY ADMIDATE

/* urinary infections */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vHES_APC_Flat]
WHERE DIAG3_01 = 'N39'
AND EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
GROUP BY ADMIDATE

/* covid */

SELECT ADMIDATE,
COUNT (*) admissions
FROM HES_APC.dbo.[vHES_APC_Flat]
WHERE DIAG3_01 = 'U07'
AND EPIORDER = 1
AND EPISTAT = 3
AND ADMIMETH IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D')
GROUP BY ADMIDATE
