USE [MONITO]
GO

/****** Object:  StoredProcedure [dbo].[Prc_GBM_GeneraOrderRF_RealTime]    Script Date: 14-10-2022 11:39:32 ******/
IF EXISTS (SELECT * FROM sys.procedures WHERE name = 'Prc_GBM_GeneraOrderRF_RealTime' AND type = 'P') 
DROP PROCEDURE [dbo].[Prc_GBM_GeneraOrderRF_RealTime]
GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





CREATE     PROCEDURE [dbo].[Prc_GBM_GeneraOrderRF_RealTime]
(@fechaConsulta DATE)
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

SET @ultimoDia = @fechaConsulta


--REPROCESO
TRUNCATE TABLE dbo.Tbl_GBM_OrderRF_Realtime_Temp
--ALIAS CLIENTES
SELECT * INTO dbo.[#Tbl_GBM_ClientAlias_RF]
FROM (SELECT DISTINCT rut_cli, per_id, ALIAS1 AS alias
		FROM NUMERAL.DBO.CUENTA_ALIAS
		WHERE TRIM(ALIAS1) <>'' AND ALIAS1 IS NOT NULL
		UNION
		SELECT DISTINCT rut_cli, per_id, ALIAS2 AS alias
		FROM NUMERAL.DBO.CUENTA_ALIAS
		WHERE TRIM(ALIAS2) <>'' AND ALIAS2 IS NOT NULL
		UNION
		SELECT DISTINCT rut_cli, per_id, ALIAS3 AS alias
		FROM NUMERAL.DBO.CUENTA_ALIAS
		WHERE TRIM(ALIAS3) <>'' AND ALIAS3 IS NOT NULL
		UNION
		SELECT DISTINCT rut_cli, per_id, ALIAS4 AS alias
		FROM NUMERAL.DBO.CUENTA_ALIAS
		WHERE TRIM(ALIAS4) <>'' AND ALIAS4 IS NOT NULL
		UNION
		SELECT DISTINCT rut_cli, per_id, ALIAS5 AS alias
		FROM NUMERAL.DBO.CUENTA_ALIAS
		WHERE TRIM(ALIAS5) <>'' AND ALIAS5 IS NOT NULL
	) A


--GUARDAR LO QUE VA DEL DIA 
SELECT * INTO dbo.[#Tbl_GBM_OrderRF_RealTime_Temp]
FROM [dbo].[Tbl_GBM_OrderRF_RealTime]
WHERE fecha_proceso = @ultimoDia

--OBTENER DATOS DE TBL_FIX OrdType
CREATE TABLE dbo.[#Tbl_Fix_ValorOrdType]
(
 Nombre_Valor VARCHAR(50),
 Descripcion_Valor VARCHAR(150),
 Id_Campo INT
)
;WITH FixCreacionOrdType AS (SELECT Nombre_Valor,Descripcion_Valor,Id_Campo   from Tbl_Fix_Valor FIX where FIX.ID_CAMPO='40')
INSERT INTO dbo.[#Tbl_Fix_ValorOrdType]
Select * from FixCreacionOrdType

--OBTENER DATOS DE TBL_FIX OrdStatus
CREATE TABLE dbo.[#Tbl_Fix_OrdStatus]
(
 Nombre_Valor VARCHAR(50),
 Descripcion_Valor VARCHAR(150),
 Id_Campo INT
)

;WITH FixCreacionOrdStatus AS (SELECT Nombre_Valor,Descripcion_Valor,Id_Campo   from Tbl_Fix_Valor FIX where FIX.ID_CAMPO='39')
INSERT INTO dbo.[#Tbl_Fix_OrdStatus]
Select * from FixCreacionOrdStatus



CREATE INDEX IDX_GBM_OrderRF_Temp_1 ON #Tbl_GBM_OrderRF_RealTime_Temp(fecha_proceso)
CREATE INDEX IDX_GBM_OrderRF_Temp_2 ON #Tbl_GBM_OrderRF_RealTime_Temp(llave)
--OBTENER ENDTIME
Select B.orderEndTime,B.orderid INTO #Tbl_TEMP_ENDTIME
From (SELECT orderid,MAX(ordFix.transacttime) AS orderEndTime
  FROM [DROPCOPY].[dbo].TBEXECUTIONREPORT_RFCORR  ordFix WITH (NOLOCK) 
    GROUP BY orderid) AS B

--GUARDAR LO QUE VA DEL DIA EN DROPCOPY
;WITH dropcopy AS (SELECT	@ultimoDia				AS fecha_proceso		
		,ordFix.transacttime
		,ordFix.ordstatus							
		,ordFix.orderId								
		,ordFix.side								
		,ordFix.securitytype						
		,ordFix.securityExchange					
		,ordFix.price							
		,ordFix.orderQtyData											
		,ordFix.leavesqty							
		,ordFix.ordtype								
		,ordFix.timeinforce
		,ordFix.clientID	
		,ordFix.exectype
		--,'Account Type'
		,ordFix.executingfirm						
		,ordfix.tradeId								
		--,'Trader Name'
		,ordFix.enteringfirm						
		--,''Counterparty Name''
		,ordFix.currency
		,ordFix.enteringtrader						
		,ordFix.secondaryOrderId
		,ordFix.texto
		,ordFix.cumqty
		,CONCAT(ordFix.transacttime,ordFix.ordstatus,ordFix.orderId) AS 'Llave'
		,ordFix.lastqty
		,ordFix.symbol
		,ordfix.settldate
		,ordFix.lastpx
		,ordfix.price2
		,orDFIX.tradedate
	FROM [DROPCOPY].[dbo].[TBEXECUTIONREPORT_RFCORR]  ordFix WITH (NOLOCK)
	WHERE CAST(STUFF(ordFix.transacttime, 11,1, ' ') AS DATE) = @ultimoDia
)	
SELECT * INTO [dbo].[#Tbl_Dropcopy_TBEXECUTIONREPORT_RF_Temp]
FROM dropcopy 

CREATE INDEX IDX_GBM_Dropcopy_Temp_1 ON #Tbl_Dropcopy_TBEXECUTIONREPORT_RF_Temp(fecha_proceso)
CREATE INDEX IDX_GBM_Dropcopy_Temp_2 ON #Tbl_Dropcopy_TBEXECUTIONREPORT_RF_Temp(llave)


--CREA CONJUNTO DE NOVEDADES
INSERT INTO [dbo].[Tbl_GBM_OrderRF_RealTime_temp]
           ([fecha_proceso]
           ,[OrderStart Date and Time]
           ,[OrderEnd Date and Time]
		   ,[OrderStart Date and Time System]
           ,[OrderEnd Date and Time System]
           ,[Order Status]
           ,[Order ID]
	       ,[BuySell] 
	       ,[AssetType] 
	       ,[ExchangeCode] 
	       ,[Currency] 
	       ,[LimitPrice] 
	       ,[ExecutionPrice] 
	       ,[BuyAmount/SellAmount] 
	       ,[Price] 
	       ,[Amount]
	       ,[Remaining Amount] 
	       ,[Book] 
	       ,[IsClient/IsInternal]
		   ,[Trader]
	       ,[Trader Name] 
	       ,[BuySettlementDate/SellSettlementDate]
		   ,[BuySettlementDate/SellSettlementDate System] 
	       ,[Counterparty Code] 
	       ,[Counterparty Name] 
	       ,[Order Type]
	       ,[TradeID]
		   ,[ISIN]
		   ,[OrderValidityPeriod]
		   ,[InstrumentID]
		   ,[Instrument_Category]
           ,[Llave]) 
SELECT	@ultimoDia									                                                                                                                               AS fecha_proceso		

		,FORMAT(CAST(STUFF(ordFix.transacttime, 11,1, ' ') AS DATETIME2) AT TIME ZONE 'Pacific SA Standard Time' AT TIME ZONE 'UTC','yyyy-MM-ddThh:mm:ss.fffZ')                    AS 'OrderStart Date and Time' 
		,FORMAT(CAST(STUFF(etim.orderEndTime, 11,1, ' ')   AS DATETIME2) AT TIME ZONE 'Pacific SA Standard Time' AT TIME ZONE 'UTC' ,'yyyy-MM-ddThh:mm:ss.fffZ')                   AS 'OrderEnd Date and Time'
		,STUFF(ordFix.transacttime, 11,1, ' ')                                                                                                                                     AS 'OrderStart Date and Time System' 
		,STUFF(etim.orderEndTime, 11,1, ' ')                                                                                                                                       AS 'OrderEnd Date and Time System'
		,CASE WHEN ordFix.cumqty > 0 THEN
			CASE WHEN ordFix.cumqty - ordFix.orderQtyData < 0 THEN 'PartiallyFilled'
				 ELSE CASE WHEN ordFix.cumqty - ordFix.orderQtyData = 0 THEN 'Filled' 
					ELSE '' END
			END

		ELSE CASE WHEN Fix_Status.Nombre_Valor IS NOT NULL 
			THEN Fix_Status.Descripcion_Valor
			ELSE ''
		END	END		                                                                                                                                                               AS 'Order Status'
		,ordFix.orderId								                                                                                                                               AS 'Order ID'
		,CASE ordFix.side
			WHEN '1' THEN 'Buy'
			WHEN '2' THEN 'Sell'
			WHEN '5' THEN 'Short'	
			ELSE '' END							                                                                                                                                   AS 'BuySell'

		,(CASE WHEN ordFix.securityType ='IRF' THEN
                           (
                             (CASE WHEN  SUBSTRING(ordFix.Symbol,1,3)='BTP' OR SUBSTRING(ordFix.Symbol,1,3)='BTU'
                                                                           OR SUBSTRING(ordFix.Symbol,1,3)='BCP'
                                                                           OR SUBSTRING(ordFix.Symbol,1,3)='BCU'
                                                                           OR SUBSTRING(ordFix.Symbol,1,3)='BVL' THEN'Cash-FixedIncome-GovernmentBond'
                                                                                                        
                                  WHEN SUBSTRING(ordFix.Symbol,1,4)='PCR-' OR SUBSTRING(ordFix.Symbol,1,4)='CERO' THEN'NonCash-LoansandDeposits-Loan'
                                ELSE 'Cash-FixedIncome-Corporate'
                             END)
                           )
                     ELSE  
                    (CASE WHEN ordFix.securityType='IIF' THEN
                             (CASE
                                    WHEN SUBSTRING(ordFix.Symbol,1,4)='BNPD' THEN'NonCash-LoansandDeposits-Loan'
                                    WHEN SUBSTRING(ordFix.Symbol,1,2)='FU' OR SUBSTRING(ordFix.Symbol,1,2)='FN'
                                                                           OR SUBSTRING(ordFix.Symbol,1,2)='F*'
                                                                           OR SUBSTRING(ordFix.Symbol,1,4)='SN' THEN'Cash-FixedIncome-MoneyMarket-CertificateofDeposit'
                                END)
                     END)
                    END)						                                                                                                                                   AS 'AssetType'
		,ordFix.securityExchange					                                                                                                                               AS 'ExchangeCode'
        ,CASE WHEN TRIM(ordFix.currency)='UF' THEN 'CLF'																														   
				 WHEN TRIM(ordFix.currency)='USD'  THEN 'USD' 																													   
				 WHEN TRIM(ordFix.currency)='CLP'  THEN 'CLP' 																													   
					ELSE '' END     	                                                                                                                           AS 'Currency'
																																												   
		,CASE WHEN OrdFix.price2 IS NULL THEN CAST(OrdFix.price AS VARCHAR(50))																									   
			ELSE CAST(OrdFix.price2 AS VARCHAR(50))																																   
			END                                                                                                                                                                    AS 'LimitPrice'
		,CAST(OrdFix.lastpx AS VARCHAR(50)) 	                                                                                                                                   AS 'ExecutionPrice'
	    ,CAST(ordfix.lastqty AS VARCHAR(50))                                                                                                                                       AS 'BuyAmount/SellAmount' 
		,ordFix.price								                                                                                                                               AS 'Price'
	    ,ordFix.orderQtyData                                                                                                                                                       AS 'Amount'
	    ,ordFix.leavesqty                                                                                                                                                          AS 'Remaining Amount'
	  

		,CASE WHEN alias.per_id IS NULL THEN ''
			ELSE CONCAT(alias.per_id,'-',ALIAS_C.cuenta)
			END                                                                                                                                                                    AS 'Book'
	    ,CASE WHEN ordFix.executingfirm = ordFix.enteringfirm THEN 'Internal'
			ELSE 'External'	END                                                                                                                                                    AS 'IsClient/IsInternal'

		 ,CASE WHEN TRIM(ordFix.enteringtrader) <> '999' THEN ordFix.enteringtrader
		 ELSE ordFix.enteringfirm	END					                                                                                                                           AS 'Trader'

		  ,CASE WHEN TRIM(ordFix.enteringtrader) <> '999' 
			THEN COALESCE(ope.ope_nombre,CAST(ordfix.enteringtrader as varchar(80)),'')
		 ELSE COALESCE(cor2.cor_nombre,ordFix.enteringfirm,'') END                                                                                                                 AS 'Trader Name'
		 ,FORMAT(CAST(ordFix.settldate AS DATETIME2)AT TIME ZONE 'Pacific SA Standard Time' AT TIME ZONE 'UTC','yyyy-MM-ddThh:mm:ss.fffZ')                                         AS 'BuySettlementDate/SellSettlementDate'
		  ,ordFix.settldate                                                                                                                                                        AS 'BuySettlementDate/SellSettlementDate System'
	     ,'085'						                                                                                                                               					AS 'Counterparty Code'
	     ,'area'                                                                                                                       			AS 'Counterparty Name' 
		     ,CASE WHEN FIX.Nombre_Valor IS NOT NULL 
			 THEN FIX.Descripcion_Valor
			  ELSE ordFix.ordtype + ' - Unknown' END     
                                                                                                                                                                                   AS 'Order Type' 
		  --,ordfix.tradeId                                                                                                                                                          AS 'TradeID' 
		           ,CONCAT(ISNULL(ordfix.tradeId,'')
                        ,CASE WHEN ISNULL(ordfix.tradeId,'') <> ''
                              THEN CONCAT('-',ordfix.tradedate)
                              ELSE ''
                              END)                                                  AS 'TradeID' 
		  ,IND.CODIGO_ISIN                                                                                                                                                       AS 'ISIN'
		  ,ordfix.timeinforce                                                                                                                                                      AS 'OrderValidityPeriod'
		  ,ordfix.symbol                                                                                                                                                           AS 'InstrumentID'																	                                                               
		  ,'Fixed Income'									                                                                                                                       AS 'Instrument_Category'
		  ,OrdFix.Llave

		
	FROM #Tbl_Dropcopy_TBEXECUTIONREPORT_RF_Temp OrdFix
	LEFT JOIN [dbo].[#Tbl_GBM_OrderRF_RealTime_Temp] ordRT 
		ON  ordRT.Llave = OrdFix.Llave
	LEFT JOIN [dbo].[#Tbl_GBM_ClientAlias_RF] alias
		ON alias.alias = OrdFix.clientID
	LEFT JOIN [MONITO].[dbo].[Tbl_COR_RutRelacionado] rel
		ON rel.rut = alias.rut_cli
	LEFT JOIN [NUMERAL].[dbo].[EMPLEADO_CORREDORA] emp
		ON emp.per_id = alias.per_id
	LEFT JOIN [MONITO].[dbo].[Tbl_CodigoOperadorMesa] ope
		ON ope.ope_codigo = ordfix.enteringtrader
	LEFT JOIN [MONITO].[dbo].[Tbl_COR_Corredores] cor
		ON ordFix.executingfirm = cor.cor_codigo
	LEFT JOIN [MONITO].[dbo].[Tbl_COR_Corredores] cor2
		ON ordFix.enteringfirm = cor2.cor_codigo
	LEFT JOIN [dbo].[#Tbl_TEMP_ENDTIME] etim
		ON ordFix.orderId=etim.orderid
	LEFT JOIN [MONITO].[dbo].[Tbl_IDENTIFICADOR] IND
		ON IND.NOMBRE_EMPRESA=OrdFix.Symbol
	LEFT JOIN [NUMERAL].[DBO].[CUENTA_ALIAS] ALIAS_C 
		ON ordfix.clientID=ALIAS_C.alias1 or ordfix.clientID=ALIAS_C.alias2 or  ordfix.clientID=ALIAS_C.alias3 or  ordfix.clientID=ALIAS_C.alias4
	LEFT JOIN [dbo].[#Tbl_Fix_ValorOrdType] Fix 
		ON ordFix.ordtype=FIX.Nombre_Valor
	LEFT JOIN [dbo].[#Tbl_Fix_OrdStatus] Fix_Status 
		ON ordFix.ordstatus=Fix_Status.Nombre_Valor 	
	WHERE ordRT.Llave IS NULL

--GRABA NOVEDADES
	INSERT INTO [dbo].[Tbl_GBM_OrderRF_RealTime]
	 ([fecha_proceso]
           ,[OrderStart Date and Time]
           ,[OrderEnd Date and Time]
           ,[OrderStart Date and Time System]
           ,[OrderEnd Date and Time System]
           ,[Order Status]
           ,[Order ID]
	       ,[BuySell] 
	       ,[AssetType] 
	       ,[ExchangeCode] 
	       ,[Currency] 
	       ,[LimitPrice] 
	       ,[ExecutionPrice] 
	       ,[BuyAmount/SellAmount] 
	       ,[Price] 
	       ,[Amount]
	       ,[Remaining Amount] 
	       ,[Book] 
	       ,[IsClient/IsInternal]
		   ,[Trader]
	       ,[Trader Name] 
	       ,[BuySettlementDate/SellSettlementDate] 
		   ,[BuySettlementDate/SellSettlementDate System]
	       ,[Counterparty Code] 
	       ,[Counterparty Name] 
	       ,[Order Type]
	       ,[TradeID]
		   ,[ISIN]
		   ,[OrderValidityPeriod]
		   ,[InstrumentID]
		   ,[Instrument_Category]
           ,[Llave])
	SELECT [fecha_proceso]
           ,[OrderStart Date and Time]
           ,[OrderEnd Date and Time]
		   ,[OrderStart Date and Time System]
           ,[OrderEnd Date and Time System]
           ,[Order Status]
           ,[Order ID]
	       ,[BuySell] 
	       ,[AssetType] 
	       ,[ExchangeCode] 
	       ,[Currency] 
	       ,[LimitPrice] 
	       ,[ExecutionPrice] 
	       ,[BuyAmount/SellAmount] 
	       ,[Price] 
	       ,[Amount]
	       ,[Remaining Amount] 
	       ,[Book] 
	       ,[IsClient/IsInternal]
		   ,[Trader]
	       ,[Trader Name] 
	       ,[BuySettlementDate/SellSettlementDate]
		   ,[BuySettlementDate/SellSettlementDate System] 
	       ,[Counterparty Code] 
	       ,[Counterparty Name] 
	       ,[Order Type]
	       ,[TradeID]
		   ,[ISIN]
		   ,[OrderValidityPeriod]
		   ,[InstrumentID]
		   ,[Instrument_Category]
           ,[Llave]
		FROM [dbo].[Tbl_GBM_OrderRF_RealTime_Temp]


 DROP TABLE [dbo].[#Tbl_GBM_OrderRF_RealTime_Temp]
 DROP TABLE [dbo].[#Tbl_Dropcopy_TBEXECUTIONREPORT_RF_Temp]
 DROP TABLE [dbo].[#Tbl_GBM_ClientAlias_RF]
 DROP TABLE [dbo].[#Tbl_Fix_ValorOrdType]
 DROP TABLE [dbo].[#Tbl_Fix_OrdStatus]
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


GRANT EXECUTE ON OBJECT::[dbo].[Prc_GBM_GeneraOrderRF_RealTime] TO usr
GO
