if exists (select * from sysobjects where id = object_id(N'[dbo].[ATUALIZAITENSEXCLUIDOSCADPRODUTOS]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[ATUALIZAITENSEXCLUIDOSCADPRODUTOS]
GO

if exists (select * from sysobjects where id = object_id(N'[dbo].[CadProdutos]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
drop table [dbo].[CadProdutos]
GO

CREATE TABLE [dbo].[CadProdutos] (
	[Codigo] [varchar] (20) NOT NULL ,
	[CodigoBarras] [varchar] (20) NOT NULL ,
	[Descricao] [varchar] (50) NOT NULL ,
	[DescricaoRedusida] [varchar] (25) NULL ,
	[CodFamilia] [int] NULL ,
	[DescricaoFamilia] [varchar] (50) NULL ,
	[CodCategoria] [int] NULL ,
	[DescricaoCategoria] [varchar] (50) NULL ,
	[CodSubCategoria] [int] NULL ,
	[DescricaoSubCategoria] [varchar] (50) NULL ,
	[CodMarca] [int] NULL ,
	[DescricaoMarca] [varchar] (50) NULL ,
	[Balanca] [varchar] (3) NULL ,
	[BalancaTecla] [varchar] (10) NULL ,
	[BalancaValidade] [varchar] (10) NULL ,
	[BalancaSetor] [varchar] (50) NULL ,
	[Promocao] [varchar] (3) NULL ,
	[PromocaoDataInicial] [datetime] NULL ,
	[PromocaoDataFinal] [datetime] NULL ,
	[PromocaoPreco] [real] NULL ,
	[Unidade] [varchar] (2) NULL ,
	[Fabricante] [varchar] (50) NULL ,
	[CodigoFornecedor] [varchar] (20) NULL ,
	[Fornecedor] [varchar] (50) NULL ,
	[EstoqueMinimo] [float] NULL ,
	[EstoqueAtual] [float] NULL ,
	[EstoqueIdeal] [float] NULL ,
	[PrecoCusto] [real] NULL ,
	[PrecoVenda] [real] NULL ,
	[IcmsEntrada] [varchar] (20) NULL ,
	[IcmsSaida] [varchar] (20) NULL ,
	[ProdutoMonofasico] [varchar] (3) NULL ,
	[ProdutoCotacao] [varchar] (3) NULL ,
	[DataUltimaAlteracao] [datetime] NULL ,
	[MatriculaUsuario] [varchar] (20) NULL ,
	[NomeUsuario] [varchar] (20) NULL ,
	[Obs] [text] NULL ,
	[GondolaCodigo] [varchar] (20) NULL ,
	[GondolaDescricao] [varchar] (50) NULL ,
	[MargemLucro] [float] NULL ,
	[TotalPorcentagemCusto] [float] NULL ,
	[Custos] [text] NULL ,
	[Cst] [varchar] (10) NULL ,
	[ClassificacaoFiscal] [varchar] (10) NULL ,
	[Cfop] [varchar] (10) NULL ,
	[PrecoCustoFinal] [real] NULL ,
	[UltimoPrecoVenda] [real] NULL ,
	[Excecao] [int] NULL ,
	[DescricaoNCM] [varchar] (80) NULL ,
	[QTDEmbalagem] [float] NULL ,
	[Desconto] [float] NULL ,
	[Comissao] [float] NULL ,
	[Pis] [float] NULL ,
	[Cofins] [float] NULL ,
	[CaixaValor] [real] NULL ,
	[CaixaQuantidade] [float] NULL ,
	[TerminalUltimaOper] [varchar] (50) NULL 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[CadProdutos] WITH NOCHECK ADD 
	CONSTRAINT [PK_CadProdutos] PRIMARY KEY  NONCLUSTERED 
	(
		[Codigo],
		[CodigoBarras],
		[Descricao]
	)  ON [PRIMARY] 
GO

SET QUOTED_IDENTIFIER  ON    SET ANSI_NULLS  ON 
GO

CREATE TRIGGER [ATUALIZAITENSEXCLUIDOSCADPRODUTOS] ON dbo.CadProdutos 
FOR DELETE 
AS

DECLARE @Codigo varchar(50) 


-- Definindo o valor das variáveis 
SELECT @Codigo = Codigo  FROM  DELETED 

-- Note que utilizamos a tabela temporária DELETED que contém os dados do DELETED Corrente 



--Vamos alterar a tabela de clientes 
Insert into ITENSEXCLUIDOS ( Codigo , DataHora , Origem ) values ( @Codigo , GetDate() , 'CADPRODUTOS'  )
--Fim da Trigger

GO
SET QUOTED_IDENTIFIER  OFF    SET ANSI_NULLS  ON 
GO

