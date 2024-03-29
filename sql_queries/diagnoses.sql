SELECT EPIKEY, DiagCode, DiagIdx
FROM HES_APC.dbo.[vtHES_APC_DIAG]
WHERE EPIKEY IN (SELECT k.EPIKEY
				 FROM HES_APC.dbo.[vtHES_APC_DIAG] AS k
				 WHERE (k.DiagCode4 LIKE 'L02%'    /* Abscess */
				OR k.DiagCode4 LIKE 'L03%' /* Cellulitis */
				OR k.DiagCode4 LIKE 'I80%' /* Phlebitis & thrombophlebitis */
				OR k.DiagCode4 = 'A480' /* Other SSTI */
				OR k.DiagCode4 LIKE 'L08%'
				OR k.DiagCode4 LIKE 'L97%'
				OR k.DiagCode4 = 'L984'
				OR k.DiagCode4 LIKE 'R02%'
				OR k.DiagCode4 = 'B376' /* Endocarditis */
				OR k.DiagCode4 LIKE 'I33%'
				OR k.DiagCode4 LIKE 'I38%'
				OR k.DiagCode4 LIKE 'I39%'
				OR k.DiagCode4 LIKE 'A40%' /* Septicaemia */
				OR k.DiagCode4 LIKE 'A41%'
				OR k.DiagCode4 = 'R572'
				OR k.DiagCode4 = 'B377'
				OR k.DiagCode4 LIKE 'M00%' /* Osteomyelitis & septic arthritis */
				OR k.DiagCode4 LIKE 'M01%' 
				OR k.DiagCode4 LIKE 'M02%' 
				OR k.DiagCode4 LIKE 'M03%' 
				OR k.DiagCode4 LIKE 'M86%' 
				OR k.DiagCode4 = 'M465'
				OR k.DiagCode4 = 'M726') /* Necrotising fasciitis */
				AND k.DiagIdx = 1
				)
AND EPIKEY IN (SELECT j.EPIKEY
					FROM HES_APC.dbo.[vtHES_APC_DIAG] AS j
					WHERE j.DiagCode4 LIKE 'F11%'
					AND j.DiagIdx BETWEEN 2 AND 20)
