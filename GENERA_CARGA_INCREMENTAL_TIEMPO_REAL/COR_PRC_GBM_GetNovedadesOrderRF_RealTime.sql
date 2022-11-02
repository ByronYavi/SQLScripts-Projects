USE [MONITO]
GO

/****** Object:  StoredProcedure [dbo].[Prc_GBM_GetNovedadesOrderRF_RealTime]    Script Date: 14-10-2022 11:39:32 ******/
IF EXISTS (SELECT * FROM sys.procedures WHERE name = 'Prc_GBM_GetNovedadesOrderRF_RealTime' AND type = 'P') 
DROP PROCEDURE [dbo].[Prc_GBM_GetNovedadesOrderRF_RealTime]
GO
/****** Object:  StoredProcedure [dbo].[Prc_GBM_GetNovedadesOrderRF_RealTime]    Script Date: 14-10-2022 19:59:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO








CREATE     PROCEDURE [dbo].[Prc_GBM_GetNovedadesOrderRF_RealTime]
AS

SET NOCOUNT ON; 
BEGIN

	DECLARE	
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

	SELECT CAST(CONCAT_WS(','
           ,'OrderStartDateandTime'
           ,'OrderEndDateandTime'
		   ,'OrderStartDateandTime System'
           ,'OrderEndDateandTime System'
           ,'OrderStatus'
           ,'OrderID'
	       ,'BuySell'
	       ,'AssetType' 
	       ,'ExchangeCode' 
	       ,'Currency' 
	       ,'LimitPrice'
	       ,'ExecutionPrice' 
	       ,'BuySellAmount' 
	       ,'Price'
	       ,'Amount'
	       ,'RemainingAmount'
	       ,'Book'
	       ,'IsClientOrIsInternal'
		   ,'Trader'
		   ,'Trader_Name'
	       ,'BuySellSettlementDate'
		   ,'BuySellSettlementDate System'
	       ,'CounterpartyCode' 
	       ,'CounterpartyName' 
	       ,'OrderType'
	       ,'TradeID'
		   ,'ISIN'
		   ,'OrderValidityPeriod'
		   ,'InstrumentID'
		   ,'Instrument_Category'
	) AS VARCHAR(8000)) AS salida
UNION ALL
	SELECT CAST(CONCAT_WS(','
           ,ISNULL([OrderStart Date and Time],'')
           ,ISNULL([OrderEnd Date and Time],'')
           ,ISNULL([OrderStart Date and Time System],'')
           ,ISNULL([OrderEnd Date and Time System],'')
           ,ISNULL([Order Status],'')
           ,ISNULL([Order ID],'')
           ,ISNULL([BuySell],'')
           ,ISNULL([AssetType],'')
           ,ISNULL([ExchangeCode],'')
		   ,ISNULL(CAST([Currency] AS VARCHAR(50)),'')		
		   ,ISNULL(CAST([LimitPrice] AS VARCHAR(50)),'')			
		   ,ISNULL(CAST([ExecutionPrice] AS VARCHAR(50)),'')	
		   ,ISNULL(CAST([BuyAmount/SellAmount] AS VARCHAR(50)),'')		
           ,ISNULL([Price],'')
		   ,ISNULL([Amount],'')
		   ,ISNULL([Remaining Amount],'')
		   ,ISNULL([Book],'')
    	   ,ISNULL([IsClient/IsInternal],'')
		   ,ISNULL([trader],'')
           ,ISNULL(LEFT(LEFT([Trader Name],5) + REPLICATE('*',20),20),'')
           ,ISNULL([BuySettlementDate/SellSettlementDate],'')
		   ,ISNULL([BuySettlementDate/SellSettlementDate System],'')
           ,ISNULL([Counterparty Code],'')
           ,ISNULL(LEFT(LEFT([Counterparty Name],5) + REPLICATE('*',20),20),'')
           ,ISNULL([Order Type],'')
           ,ISNULL([TradeID],'')
		   ,ISNULL([ISIN],'')
		   ,ISNULL([OrderValidityPeriod],'')
		   ,ISNULL([InstrumentID],'')
		   ,ISNULL([Instrument_Category],'')
          
	) AS VARCHAR(8000)) AS salida      
	FROM [dbo].[Tbl_GBM_OrderRF_RealTime_temp]


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


GRANT EXECUTE ON OBJECT::[dbo].[Prc_GBM_GetNovedadesOrderRF_RealTime] TO usr
GO