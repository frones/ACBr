if exists (select * from sysobjects where id = object_id(N'[dbo].[CADASTROCOMANDAS]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[CADASTROCOMANDAS]
GO

CREATE TABLE [dbo].[CADASTROCOMANDAS] (
	[CodigoBarras] [varchar] (30) NOT NULL ,
	[CodigoDigitacao] [varchar] (4) NULL ,
	[Ativa] [varchar] (3) NULL ,
	[DataCadastro] [datetime] NULL 
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[CADASTROCOMANDAS] WITH NOCHECK ADD 
	 PRIMARY KEY  CLUSTERED 
	(
		[CodigoBarras]
	)  ON [PRIMARY] 
GO

