USE [DESORIGEN]
GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO






CREATE PROCEDURE [dbo].[Prc_Movimiento_DI]
@fechaConsulta DATE
AS

SET NOCOUNT ON; 
BEGIN

	DECLARE @ultimoDia     DATE,		
		-- ERROR VARIABLES
		@SUSER_SNAME     NVARCHAR(128), 
		@ERROR_NUMBER    INT, 
		@ERROR_SEVERITY  INT, 
		@ERROR_STATE     INT,
		@ERROR_LINE      INT, 
		@ERROR_MESSAGE   NVARCHAR(4000), 
		@ERROR_PROCEDURE NVARCHAR(128),
		@FECHA_ERROR     DATETIME

BEGIN TRY

SET @ultimoDia = dbo.Fun_UltimoDiaHabil(@fechaConsulta)
--REPROCESO
DELETE FROM dbo.Tbl_OPERACIONES_BI
WHERE reportDate = FORMAT(@fechaConsulta,'yyyyMMdd')


--REPROCESO
TRUNCATE TABLE dbo.Tbl_OPERACIONES_BI_temp
--GRABAR DATOS DEL DIA

 CREATE TABLE dbo.[#TMPEJECUTIVOS]
(
 PER_ID VARCHAR(50),
  emp_codigo VARCHAR(50),
   nombre_emp VARCHAR(50)
)
 ;WITH TMPEJECUTIVOS AS (
  SELECT PER.PER_ID, EMP.emp_codigo, isnull((PER.per_paterno + ' ' + PER.per_Materno + ' ' + PER.per_nombres ), '') AS nombre_emp  
  FROM [CAREXTER].[dbo].[PERSONA] PER 
	INNER JOIN [CAREXTER].[dbo].[EMPLEADO_CORREDORA] EMP ON (PER.PER_ID=EMP.per_id)

 )
 INSERT INTO [#TMPEJECUTIVOS]
	SELECT * FROM TMPEJECUTIVOS

;WITH Salidas AS (
		SELECT DISTINCT  
				  FORMAT(@fechaConsulta,'yyyyMMdd')                                                              AS reportDate,
				  --@fechaConsulta                                                                                     AS reportDate,
				  FIF.fec_trans							                                                         AS FechaMovimiento,
				  FIF.TIPO_COMP_FAC                                                                              AS TipoMovimiento,
				  --RTRIM(LOWER(CONVERT(VARCHAR(32), HashBytes('md5', TRIM(FIF.RUT_DEST)), 2)))                    AS RutCliente,
				  FIF.RUT_DEST                    AS RutCliente,
				  --cue.RUT_CLI                                    AS Rut_Cliente,			                     
				  CONVERT(VARCHAR(3), FIF.SEC_RUT_DEST)								                             AS Cuenta,
				  pe.par_seg_id								                                                     AS Segmento,
				  di.din_valor								                                                     AS TipoCliente,
				  --concat(cli.emp_codigo,' ',EJE.nombre_emp)                                                      AS Ejecutivo,
				  EJE.nombre_emp                                                                                 AS Ejecutivo,
				  'CATEGORIA X'                                                                           AS Area,
				  ope.NUMERO_OPERACION							                                                 AS NumeroOperacion,
				  FIF.FOLIO_COMP_FAC                                                                             AS NumeroFactura,
				  OPE.NEMOTECNICO                                                                                AS NemotecnicoInstrumento,
				    CASE 
					WHEN FIF.PRODUCTO IS NOT NULL THEN FIF.PRODUCTO
					--WHEN FIF.PRODUCTO IS NULL AND MONE.FOLIO_OPERACION IS NOT NULL THEN 'MDOL'	
					ELSE 'Unknow'
				  END	                                                                                         AS Producto,	                             
				  Ope.moneda                                                                                     AS Moneda,
				  FIF.cantidad							                                                         AS CantidadNocionales,
				  FIF.MONTO								                                                         AS Monto,
				  FIF.COMISION                                                                                   AS UtilidadComision,
				  FIF.TIPO_OPERAC                                                                                AS TipoOperacion,
				  OPE.NUMERO_OPERADOR                                                                            AS OperadorOperacion,
				  FIF.TIPO_COMP_FAC                                                                              AS tipoCompFac,
				  FIF.COD_MOVTO																					 AS CodigoMovimiento
			
		FROM CAREXTER.DBO.TBCTMFIF FIF 
	      LEFT JOIN DESORIGEN.DBO.MFBO_REG_OPERACION OPE ON (TRIM(FIF.RUT_DEST)=TRIM(OPE.RUT_CLIENTE)) AND TRIM(FIF.FOLIO_TRANS)=OPE.FOLIO AND FIF.FOLIO_COMP_FAC=OPE.ID_BOLSA AND FIF.SEC_RUT_DEST=TRIM(OPE.CUENTA_CLIENTE)
	      LEFT JOIN [CAREXTER].[DBO].[PERSONA] PE ON (LEFT(TRIM(FIF.RUT_DEST), CHARINDEX('-',TRIM(FIF.RUT_DEST))-1) = PE.per_id)
          LEFT JOIN [CAREXTER].[dbo].[INDICADORES_CLIENTE] ic ON (ic.per_id = PE.per_id AND ic.ind_id in(114,116))
	      LEFT JOIN [CAREXTER].[dbo].[CLIENTE] cli ON (cli.per_id = PE.per_id)
	      LEFT JOIN [CAREXTER].[DBO].[DETALLE_INDICADOR] di ON ic.ind_id = di.ind_id and ic.din_id = di.din_id
	      LEFT JOIN #TMPEJECUTIVOS EJE ON (EJE.emp_codigo=cli.emp_codigo)
		WHERE FIF.FEC_MVTO = @ultimoDia and FIF.EST_MOVTO_FAC='E'

UNION ALL

		SELECT DISTINCT  
				  FORMAT(@fechaConsulta,'yyyyMMdd')                                                              AS reportDate,
				 -- @fechaConsulta                                                                                    AS reportDate,
				  FRF.fec_trans							                                                         AS FechaMovimiento,
				  FRF.TIPO_COMP_FAC                                                                              AS TipoMovimiento,
				  RTRIM(LOWER(CONVERT(VARCHAR(32), HashBytes('md5', TRIM(FRF.RUT_DEST)), 2)))                    AS RutCliente,
				  --cue.RUT_CLI                                    AS Rut_Cliente,			                     
				  CONVERT(VARCHAR(3), FRF.SEC_RUT_DEST)								                             AS Cuenta,
				  pe.par_seg_id								                                                     AS Segmento,
				  di.din_valor								                                                     AS TipoCliente,
				  --concat(cli.emp_codigo,' ',EJE.nombre_emp)                                                      AS Ejecutivo,
				  EJE.nombre_emp                                                                                 AS Ejecutivo,
				  'CATEGORIA X'                                                                           AS Area,
				  ope.NUMERO_OPERACION							                                                 AS NumeroOperacion,
				  FRF.FOLIO_COMP_FAC                                                                             AS NumeroFactura,
				  OPE.NEMOTECNICO                                                                                AS NemotecnicoInstrumento,
				    CASE 
					WHEN FRF.PRODUCTO IS NOT NULL THEN FRF.PRODUCTO
					--WHEN FIF.PRODUCTO IS NULL AND MONE.FOLIO_OPERACION IS NOT NULL THEN 'MDOL'	
					ELSE 'Unknow'
				  END	                                                                                         AS Producto,	                             
				  Ope.moneda                                                                                     AS Moneda,
				  FRF.cantidad							                                                         AS CantidadNocionales,
				  FRF.MONTO								                                                         AS Monto,
				  FRF.COMISION                                                                                   AS UtilidadComision,
				  FRF.TIPO_OPERAC                                                                                AS TipoOperacion,
				  OPE.NUMERO_OPERADOR                                                                            AS OperadorOperacion,
				  FRF.TIPO_COMP_FAC                                                                              AS tipoCompFac,
				  FRF.COD_MOVTO																					 AS CodigoMovimiento
			
		FROM CAREXTER.DBO.TBCTMFRF FRF
	      LEFT JOIN DESORIGEN.DBO.MFBO_REG_OPERACION OPE ON (TRIM(FRF.RUT_DEST)=TRIM(OPE.RUT_CLIENTE)) AND TRIM(FRF.FOLIO_TRANS)=OPE.FOLIO AND FRF.FOLIO_COMP_FAC=OPE.ID_BOLSA AND FRF.SEC_RUT_DEST=TRIM(OPE.CUENTA_CLIENTE)
	      LEFT JOIN [CAREXTER].[DBO].[PERSONA] PE ON (LEFT(TRIM(OPE.RUT_CLIENTE), CHARINDEX('-',TRIM(OPE.RUT_CLIENTE))-1) = PE.per_id)
          LEFT JOIN [CAREXTER].[dbo].[INDICADORES_CLIENTE] ic ON (ic.per_id = PE.per_id AND ic.ind_id in(114,116))
	      LEFT JOIN [CAREXTER].[dbo].[CLIENTE] cli ON (cli.per_id = PE.per_id)
	      LEFT JOIN [CAREXTER].[DBO].[DETALLE_INDICADOR] di ON ic.ind_id = di.ind_id and ic.din_id = di.din_id
	      LEFT JOIN #TMPEJECUTIVOS EJE ON (EJE.emp_codigo=cli.emp_codigo)
		WHERE FRF.FEC_MVTO =@ultimoDia AND TRIM(FRF.EST_MOVTO_FAC)='E'

UNION ALL

		SELECT DISTINCT  
				  FORMAT(@fechaConsulta,'yyyyMMdd')                                                              AS reportDate,
				 --@fechaConsulta                                                                                 AS reportDate,
				  FRV.fec_trans							                                                         AS FechaMovimiento,
				  FRV.TIPO_COMP_FAC                                                                              AS TipoMovimiento,
				  RTRIM(LOWER(CONVERT(VARCHAR(32), HashBytes('md5', TRIM(FRV.RUT_DEST)), 2)))                    AS RutCliente,
				  --cue.RUT_CLI                                    AS Rut_Cliente,			                     
				  CONVERT(VARCHAR(3), FRV.SEC_RUT_DEST)								                             AS Cuenta,
				  pe.par_seg_id								                                                     AS Segmento,
				  di.din_valor								                                                     AS TipoCliente,
				  --concat(cli.emp_codigo,' ',EJE.nombre_emp)                                                      AS Ejecutivo,
				  EJE.nombre_emp                                                                                 AS Ejecutivo,
				  'CATEGORIA X'                                                                           AS Area,
				  ope.NUMERO_OPERACION							                                                 AS NumeroOperacion,
				  FRV.FOLIO_COMP_FAC                                                                             AS NumeroFactura,
				   OPE.NEMOTECNICO                                                                               AS NemotecnicoInstrumento,
				    CASE 
					WHEN FRV.PRODUCTO IS NOT NULL THEN FRV.PRODUCTO
					--WHEN FIF.PRODUCTO IS NULL AND MONE.FOLIO_OPERACION IS NOT NULL THEN 'MDOL'	
					ELSE 'Unknow'
				  END	                                                                                         AS Producto,	                             
				  Ope.moneda                                                                                     AS Moneda,
				  FRV.cantidad							                                                         AS CantidadNocionales,
				  CASE 
					WHEN FRV.TIPO_COMP_FAC='FT' THEN '0'
				  ELSE FRV.MONTO END						                                                     AS Monto,
				  CASE 
					WHEN FRV.TIPO_COMP_FAC='FT' THEN FRV.MONTO
				  ELSE FRV.COMISION  END	                                                                     AS UtilidadComision,

				  FRV.TIPO_OPERAC                                                                                AS TipoOperacion,
				  OPE.NUMERO_OPERADOR                                                                            AS OperadorOperacion,
				  FRV.TIPO_COMP_FAC                                                                              AS tipoCompFac,
				  FRV.COD_MOVTO																					 AS CodigoMovimiento


			
		FROM CAREXTER.DBO.TBCTMFRV FRV
	      LEFT JOIN DESORIGEN.DBO.MFBO_REG_OPERACION OPE ON (TRIM(FRV.RUT_DEST)=TRIM(OPE.RUT_CLIENTE)) AND TRIM(FRV.FOLIO_TRANS)=OPE.FOLIO AND FRV.FOLIO_COMP_FAC=OPE.ID_BOLSA AND FRV.SEC_RUT_DEST=TRIM(OPE.CUENTA_CLIENTE)
	      LEFT JOIN [CAREXTER].[DBO].[PERSONA] PE ON (LEFT(TRIM(OPE.RUT_CLIENTE), CHARINDEX('-',TRIM(OPE.RUT_CLIENTE))-1) = PE.per_id)
          LEFT JOIN [CAREXTER].[dbo].[INDICADORES_CLIENTE] ic ON (ic.per_id = PE.per_id AND ic.ind_id in(114,116))
	      LEFT JOIN [CAREXTER].[dbo].[CLIENTE] cli ON (cli.per_id = PE.per_id)
	      LEFT JOIN [CAREXTER].[DBO].[DETALLE_INDICADOR] di ON ic.ind_id = di.ind_id and ic.din_id = di.din_id
	      LEFT JOIN #TMPEJECUTIVOS EJE ON (EJE.emp_codigo=cli.emp_codigo)
		WHERE FRV.FEC_MVTO = @ultimoDia AND TRIM(FRV.EST_MOVTO_FAC)='E'
)
SELECT * INTO dbo.[#Tbl_OPERACIONES_BI_temp]
FROM Salidas



--Guardar datos del dia
 INSERT INTO [dbo].[Tbl_OPERACIONES_BI]
     (reportDate,
	FechaMovimiento,
	TipoMovimiento ,
	RutCliente ,
	Cuenta ,
	Segmento ,
	TipoCliente ,
	Ejecutivo ,
	Area ,
	NumeroOperacion ,
	NumeroFactura ,
	NemotecnicoInstrumento ,
	Producto ,
	Moneda ,
	CantidadNocionales,
	Monto ,
	UtilidadComision ,
	TipoOperacion ,
	OperadorOperacion,
	tipoCompFac,
	CodigoMovimiento)
Select * from [#Tbl_OPERACIONES_BI_temp]


--Generar Salida con las novedades
SELECT CAST(CONCAT_WS(','
	,'reportDate'
	,'FechaMovimiento'
	,'TipoMovimiento'
	,'RutCliente'
	,'Cuenta'
	,'Segmento'
	,'TipoCliente'
	,'Ejecutivo'
	,'Area'
	,'NumeroOperacion'
	,'NumeroFactura'
	,'NemotecnicoInstrumento'
	,'Producto'
	,'Moneda'
	,'CantidadNocionales'
	,'UtilidadComision' --Internal/External
	,'TipoOperacion'
	,'OperadorOperacion'
	,'tipoCompFac'
	,'CodigoMovimiento'

	) AS VARCHAR(8000)) AS salidax
	UNION ALL
	SELECT CAST(CONCAT_WS(','
	,ISNULL(reportDate,'')
	,ISNULL(FechaMovimiento,'')
	,ISNULL(TipoMovimiento ,'')
	,ISNULL(RutCliente ,'')
	,ISNULL(Cuenta,'')
	,ISNULL(Segmento,'')
	,ISNULL(TipoCliente,'')
	,ISNULL(Ejecutivo,'')
	,ISNULL(Area,'')
	,ISNULL(NumeroOperacion,'')
	,ISNULL(NumeroFactura,'')
	,ISNULL(NemotecnicoInstrumento,'')
	,ISNULL(Producto,'')
	,ISNULL(Moneda,'')
	,ISNULL(CantidadNocionales,'')
	,ISNULL(UtilidadComision,'')
	,ISNULL(TipoOperacion,'')
	,ISNULL(OperadorOperacion,'')
	,ISNULL(tipoCompFac,'')
	,ISNULL(CodigoMovimiento,'')

	) AS VARCHAR(8000)) AS salidax
	FROM dbo.#Tbl_OPERACIONES_BI_temp


DROP TABLE #Tbl_OPERACIONES_BI_temp
DROP TABLE #TMPEJECUTIVOS


END TRY
  	BEGIN CATCH
		-- REPORT EXCEPTION
		SET @SUSER_SNAME     = SUSER_SNAME() 
		SET	@ERROR_SEVERITY  = ERROR_SEVERITY()
		SET	@ERROR_NUMBER    = ERROR_NUMBER()
		SET	@ERROR_STATE     =  ERROR_STATE()
		SET	@ERROR_LINE      = ERROR_LINE()
		SET	@ERROR_PROCEDURE = ERROR_PROCEDURE()
		SET	@ERROR_MESSAGE   = ERROR_MESSAGE()
		SET	@FECHA_ERROR     = GETDATE()

		EXEC [dbo].[Prc_ReportaSPError] 
			@SUSER_SNAME, 
			@ERROR_NUMBER , 
			@ERROR_SEVERITY,	
			@ERROR_STATE,
			@ERROR_LINE,
			@ERROR_PROCEDURE,
			@ERROR_MESSAGE, 
			@FECHA_ERROR
 	END CATCH
END
GO


