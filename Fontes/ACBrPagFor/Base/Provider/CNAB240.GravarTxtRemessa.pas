{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit CNAB240.GravarTxtRemessa;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrPagForGravarTxt, ACBrPagForClass, ACBrPagForConversao;

type
  { TArquivoW_CNAB240 }

  TArquivoW_CNAB240 = class(TArquivoWClass)
  private
    FNomeArquivo: String;
    FArquivoTXT: String;

  protected
    FQtdeRegistros: Integer;
    FQtdeLotes: Integer;
    FQtdeContasConc: Integer;
    FSequencialDoRegistroNoLote: Integer;
    FQtdeRegistrosLote: Integer;
    FveRegistro1: String;
    FpFormaLancamento: TFormaLancamento;

    procedure GeraRegistro0; virtual;

    procedure GeraRegistro1(I: Integer); virtual;

    procedure GeraRegistro5(I: Integer); virtual;

    procedure GeraRegistro9; virtual;

    {
      Pagamento através de Crédito em Conta Corrente, Cheque,
      OP, DOC, Pagamento com Autenticação ou Pix
    }
    procedure GeraSegmentoA(I: Integer); virtual;

    {
      Pagamento através de Crédito em Conta Corrente, Cheque,
      OP, DOC, Pagamento com Autenticação ou Pix
    }
    procedure GeraSegmentoB(mSegmentoBList: TSegmentoBList); virtual;

    {
      Pagamento através de Crédito em Conta Corrente, Cheque,
      OP, DOC, Pagamento com Autenticação ou Pix
    }
    procedure GeraSegmentoC(mSegmentoCList: TSegmentoCList); virtual;

    {
      Pagamento de Títulos de Cobrança e QRCode Pix
    }
    procedure GeraSegmentoJ(I: Integer); virtual;

    {
      Pagamento de Títulos de Cobrança e QRCode Pix
    }
    procedure GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List); virtual;

    {
      Pagamento de Tributos
      Pagamento de Tributos sem Código de Barras
    }
    procedure GeraSegmentoN(mSegmentoN: TSegmentoN); virtual;
    // Registros de Informações complementares do Segmento N
    // (Registro de Pagamento de Tributos e Impostos sem Código de Barras)
    procedure GeraSegmentoN1(I: Integer); virtual;

    procedure GeraSegmentoN2(I: Integer); virtual;

    procedure GeraSegmentoN3(I: Integer); virtual;

    procedure GeraSegmentoN4(I: Integer); virtual;

    procedure GeraSegmentoN567(I: Integer); virtual;

    procedure GeraSegmentoN8(I: Integer); virtual;

    procedure GeraSegmentoN9(I: Integer); virtual;

    {
      Pagamento de Tributos
      Pagamento de Contas e Tributos com Código de Barras
    }
    procedure GeraSegmentoO(I: Integer); virtual;

    {
      Pagamento de Tributos
      Pagamento de Tributos sem Código de Barras
      Pagamento de Contas e Tributos com Código de Barras
    }
    procedure GeraSegmentoW(mSegmentoWList: TSegmentoWList); virtual;

    {
      Pagamento de Tributos
      Pagamento de Tributos sem Código de Barras
      Pagamento de Contas e Tributos com Código de Barras
    }
    procedure GeraSegmentoZ(mSegmentoZList: TSegmentoZList); virtual;

    procedure ValidarLinha(const Tipo: string); virtual;
    procedure IncluirLinha; virtual;

    Procedure UsarDadosConfiguracaoRegistro0;
    Procedure UsarDadosConfiguracaoRegistro1(I: Integer);
  public
    function GerarTxt: Boolean; override;

    procedure LimparRegistros;
    procedure GerarLote(I: Integer);

    property NomeArquivo: string read FNomeArquivo write FNomeArquivo;
    property ArquivoTXT: string read FArquivoTXT write FArquivoTXT;
  end;

implementation

uses
  ACBrUtil.Strings, ACBrUtil.Compatibilidade,
  ACBrPagFor, ACBrPagForProviderBase;

{ TArquivoW_CNAB240 }

procedure TArquivoW_CNAB240.GeraRegistro0;
begin
  // Em conformidade com o layout da Febraban versão 10.7
  FpLinha := '';
  FQtdeRegistros := 1;
  FQtdeLotes := 0;
  FQtdeContasConc := 0;

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(0, 4, tcInt);
  GravarCampo(0, 1, tcInt);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Registro0.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(PagFor.Registro0.Empresa.Convenio, 20, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Agencia.DV, 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.Numero, 12, tcInt64);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.ContaCorrente.DV, 1, tcStr);
  GravarCampo(PagFor.Registro0.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Registro0.NomeBanco, 30, tcStr, True);
  GravarCampo(' ', 10, tcStr);
  GravarCampo(TpArquivoToStr(PagFor.Registro0.Arquivo.Codigo), 1, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.DataGeracao, 8, tcDat);
  GravarCampo(PagFor.Registro0.Arquivo.HoraGeracao, 6, tcHor);
  GravarCampo(PagFor.Registro0.Arquivo.Sequencia, 6, tcInt);
  GravarCampo('103', 3, tcStr);
  GravarCampo(PagFor.Registro0.Arquivo.Densidade, 5, tcInt);
  GravarCampo(PagFor.Registro0.ReservadoBanco, 20, tcStr);
  GravarCampo(PagFor.Registro0.ReservadoEmpresa, 20, tcStr, True);
  GravarCampo(' ', 29, tcStr);

  ValidarLinha('0');
  IncluirLinha;
end;

procedure TArquivoW_CNAB240.GeraRegistro1(I: Integer);
var
  Versao: string;
begin
  // Em conformidade com o layout da Febraban versão 10.7
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeLotes);

  if PagFor.Lote.Items[I].Registro1.Servico.Operacao = toExtrato then
    Inc(FQtdeContasConc);

  FQtdeRegistrosLote := 1;
  FSequencialDoRegistroNoLote := 0;

  FpFormaLancamento := PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento;

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo(1, 1, tcInt);
  GravarCampo(TpOperacaoToStr(PagFor.Lote.Items[I].Registro1.Servico.Operacao), 1, tcStr);
  GravarCampo(TpServicoToStr(PagFor.Lote.Items[I].Registro1.Servico.TipoServico), 2, tcStr);
  GravarCampo(FmLancamentoToStr(FpFormaLancamento), 2, tcStr);

  case FpFormaLancamento of
    {flCartaoSalario, flCreditoContaPoupanca, flCreditoContaCorrenteMesmaTitularidade,
     flDocMesmaTitularidade, flPagamentoConcessionarias, flLiquidacaoTitulosProprioBanco,
     flLiberacaoTitulosNotaFiscalEletronica, flLiquidacaoParcelasNaoRegistrada,
     flFGTSGFIP, flExtratoContaCorrente, flTEDOutraTitularidade,
     flTEDMesmaTitularidade, flTEDTransferencia, flDebitoContaCorrente,
     flExtratoGestaoCaixa, flDepositoJudicialContaCorrente, flCartaoSalarioItau,
     flDepositoJudicialPoupanca, flExtratoContaInvestimento, flPIXTransferencia,
     flPIXQRCode, flNenhum}

    flCreditoContaCorrente, flChequePagamento, flDocTed, flOPDisposicao,
    flPagamentoAutenticacao, flPagamentoContas:
      Versao := '046';

    flLiquidacaoTitulosOutrosBancos:
      Versao := '040';

    flTributoDARFNormal, flTributoGPS, flTributoDARFSimples, flTributoIPTU,
    flTributoDARJ, flTributoGARESPICMS, flTributoGARESPDR, flTributoGARESPITCMD,
    flTributoIPVA, flTributoLicenciamento, flTributoDPVAT, flTributoGNRe:
      Versao := '012';
  else
    Versao := '000';
  end;

  GravarCampo(Versao, 3, tcStr);
  GravarCampo(' ', 1, tcStr);
  GravarCampo(TpInscricaoToStr(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Tipo), 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Numero, 14, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Convenio, 20, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo, 5, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.DV, 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero, 12, tcInt64);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV, 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.DV, 1, tcStr);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Empresa.Nome, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Informacao1, 40, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Logradouro, 30, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Numero, 5, tcStrZero);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Complemento, 15, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Cidade, 20, tcStr, True);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.CEP, 8, tcInt);
  GravarCampo(PagFor.Lote.Items[I].Registro1.Endereco.Estado, 2, tcStr);

  case PagFor.Lote.Items[I].Registro1.Servico.FormaLancamento of
    flLiquidacaoTitulosOutrosBancos:
      GravarCampo(' ', 2, tcStr);
  else
    GravarCampo(IndFormaPagToStr(PagFor.Lote.Items[I].Registro1.IndFormaPag), 2, tcStr);
  end;

  GravarCampo(' ', 6, tcStr);
  GravarCampo(' ', 10, tcStr);

  ValidarLinha('1');
  IncluirLinha;
end;

procedure TArquivoW_CNAB240.GeraRegistro5(I: Integer);
begin
  // Em conformidade com o layout da Febraban versão 10.7
  FpLinha := '';
  Inc(FQtdeRegistros);
  Inc(FQtdeRegistrosLote);

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(FQtdeLotes, 4, tcInt);
  GravarCampo('5', 1, tcStr);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(FQtdeRegistrosLote, 6, tcInt);

  case PagFor.Lote.Items[I].Registro1.Servico.TipoServico of
    tsAlegacaoSacado:
      GravarCampo(' ', 217, tcStr);
  else
    begin
      GravarCampo(PagFor.Lote.Items[I].Registro5.Valor, 18, tcDe2);
      GravarCampo(PagFor.Lote.Items[I].Registro5.QtdeMoeda, 18, tcDe5);
      GravarCampo(PagFor.Lote.Items[I].Registro5.NumAvisoDebito, 6, tcInt);
      GravarCampo(' ', 165, tcStr);
      GravarCampo(' ', 10, tcStr);
    end;
  end;

  ValidarLinha('5');
  IncluirLinha;
end;

procedure TArquivoW_CNAB240.GeraRegistro9;
begin
  // Em conformidade com o layout da Febraban versão 10.7
  FpLinha := '';
  Inc(FQtdeRegistros);

  GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
  GravarCampo(9999, 4, tcInt);
  GravarCampo('9', 1, tcStr);
  GravarCampo(' ', 9, tcStr);
  GravarCampo(FQtdeLotes, 6, tcInt);
  GravarCampo(FQtdeRegistros, 6, tcInt);
  GravarCampo(FQtdeContasConc, 6, tcInt);
  GravarCampo(' ', 205, tcStr);

  ValidarLinha('9');
  IncluirLinha;
end;

procedure TArquivoW_CNAB240.GeraSegmentoA(I: Integer);
var
  J: Integer;
begin
  // Em conformidade com o layout da Febraban versão 10.7
  for J := 0 to PagFor.Lote.Items[I].SegmentoA.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoA.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('A', 1, tcStr);
      GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo(Favorecido.Camara, 3, tcInt);
      GravarCampo(BancoToStr(Favorecido.Banco), 3, tcStr);
      GravarCampo(Favorecido.ContaCorrente.Agencia.Codigo, 5, tcInt);
      GravarCampo(Favorecido.ContaCorrente.Agencia.DV, 1, tcStr);
      GravarCampo(Favorecido.ContaCorrente.Conta.Numero, 12, tcInt64);
      GravarCampo(Favorecido.ContaCorrente.Conta.DV, 1, tcStr);
      GravarCampo(Favorecido.ContaCorrente.DV, 1, tcStr);
      GravarCampo(Favorecido.Nome, 30, tcStr, True);
      GravarCampo(Credito.SeuNumero, 20, tcStr);
      GravarCampo(Credito.DataPagamento, 8, tcDat);
      GravarCampo(TpMoedaToStr(Credito.Moeda.Tipo), 3, tcStr);
      GravarCampo(Credito.Moeda.Qtde, 15, tcDe5);
      GravarCampo(Credito.ValorPagamento, 15, tcDe2);
      GravarCampo(Credito.NossoNumero, 20, tcStr);
      GravarCampo(Credito.DataReal, 8, tcDat);
      GravarCampo(Credito.ValorReal, 15, tcDe2);
      GravarCampo(Informacao2, 40, tcStr, True);
      GravarCampo(CodigoDOC, 2, tcStr);
      GravarCampo(CodigoTED, 5, tcStr);
      GravarCampo(CodigoComp, 2, tcStr);
      GravarCampo(' ', 3, tcStr);
      GravarCampo(Aviso, 1, tcInt);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('A');
      IncluirLinha;

      {opcionais do segmento A}
      GeraSegmentoB(SegmentoB);
      GeraSegmentoC(SegmentoC);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoB(mSegmentoBList: TSegmentoBList);
var
  J: Integer;
begin
  // Em conformidade com o layout da Febraban versão 10.7
  for J := 0 to mSegmentoBList.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoBList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('B', 1, tcStr);
      GravarCampo(' ', 3, tcStr);
      GravarCampo(TpInscricaoToStr(Inscricao.Tipo), 1, tcStr);
      GravarCampo(Inscricao.Numero, 14, tcStrZero);

      case FpFormaLancamento of
        flDebitoContaCorrente:
          begin
            GravarCampo(Endereco.Logradouro, 30, tcStr, True);
            GravarCampo(Endereco.Numero, 5, tcStrZero);
            GravarCampo(Endereco.Complemento, 15, tcStr, True);
            GravarCampo(Endereco.Bairro, 15, tcStr, True);
            GravarCampo(Endereco.Cidade, 20, tcStr, True);
            GravarCampo(Endereco.CEP, 8, tcInt);
            GravarCampo(Endereco.Estado, 2, tcStr);
            GravarCampo(DataVencimento, 8, tcDat);
            GravarCampo(Valor, 15, tcDe2);
            GravarCampo(Abatimento, 15, tcDe2);
            GravarCampo(Desconto, 15, tcDe2);
            GravarCampo(Mora, 15, tcDe2);
            GravarCampo(Multa, 15, tcDe2);
            GravarCampo(CodigoDOC, 15, tcStr);
            GravarCampo(' ', 15, tcStr);
          end;
      else
        begin
          GravarCampo(Informacao10, 35, tcStr);
          GravarCampo(Informacao11, 60, tcStr);
          GravarCampo(Informacao12, 99, tcStr);
          GravarCampo(CodigoUG, 6, tcInt);
          GravarCampo(CodigoISPB, 8, tcInt);
        end;
      end;

      ValidarLinha('B');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoC(mSegmentoCList: TSegmentoCList);
var
  J: Integer;
begin
  // Em conformidade com o layout da Febraban versão 10.7
  for J := 0 to mSegmentoCList.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoCList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('C', 1, tcStr);
      GravarCampo(' ', 3, tcStr);
      GravarCampo(ValorIR, 15, tcDe2);
      GravarCampo(ValorISS, 15, tcDe2);
      GravarCampo(ValorIOF, 15, tcDe2);
      GravarCampo(Deducoes, 15, tcDe2);
      GravarCampo(Acrescimos, 15, tcDe2);
      GravarCampo(Substituta.ContaCorrente.Agencia.Codigo, 5, tcInt);
      GravarCampo(Substituta.ContaCorrente.Agencia.DV, 1, tcStr);
      GravarCampo(Substituta.ContaCorrente.Conta.Numero, 12, tcInt64);
      GravarCampo(Substituta.ContaCorrente.Conta.DV, 1, tcStr);
      GravarCampo(Substituta.ContaCorrente.DV, 1, tcStr);
      GravarCampo(ValorINSS, 15, tcDe2);
      GravarCampo(NumContaPagCreditada, 20, tcStr);
      GravarCampo(' ', 93, tcStr);

      ValidarLinha('C');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoJ(I: Integer);
var
  J: Integer;
begin
  // Em conformidade com o layout da Febraban versão 10.7
  for J := 0 to PagFor.Lote.Items[I].SegmentoJ.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoJ.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('J', 1, tcStr);
      GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo(CodigoBarras, 44, tcStr);
      GravarCampo(NomeCedente, 30, tcStr, True);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(ValorTitulo, 15, tcDe2);
      GravarCampo(Desconto, 15, tcDe2);
      GravarCampo(Acrescimo, 15, tcDe2);
      GravarCampo(DataPagamento, 8, tcDat);
      GravarCampo(ValorPagamento, 15, tcDe2);
      GravarCampo(QtdeMoeda, 15, tcDe5);
      GravarCampo(ReferenciaSacado, 20, tcStr, True);
      GravarCampo(NossoNumero, 20, tcStr);
      GravarCampo(CodigoMoeda, 2, tcInt);
      GravarCampo(' ', 6, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('J');
      IncluirLinha;

      {opcionais segmento J}
      GeraSegmentoJ52(SegmentoJ52);
//      GeraSegmentoB(SegmentoB);
//      GeraSegmentoC(SegmentoC);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List);
var
  J: Integer;
begin
  // Em conformidade com o layout da Febraban versão 10.7
  for J := 0 to mSegmentoJ52List.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoJ52List.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('J', 1, tcStr);
      GravarCampo(' ', 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo('52', 2, tcStr);
      GravarCampo(TpInscricaoToStr(Pagador.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Pagador.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Pagador.Nome, 40, tcStr, True);
      GravarCampo(TpInscricaoToStr(Beneficiario.Inscricao.Tipo), 1, tcStr);
      GravarCampo(Beneficiario.Inscricao.Numero, 15, tcStrZero);
      GravarCampo(Beneficiario.Nome, 40, tcStr, True);

      if Chave = '' then
      begin
        GravarCampo(TpInscricaoToStr(SacadorAvalista.Inscricao.Tipo), 1, tcStr);
        GravarCampo(SacadorAvalista.Inscricao.Numero, 15, tcStrZero);
        GravarCampo(SacadorAvalista.Nome, 40, tcStr, True);
        GravarCampo(' ', 53, tcStr);
      end
      else
      begin
        GravarCampo(Chave, 79, tcStr);
        GravarCampo(TXID, 30, tcStr);
      end;

      ValidarLinha('J52');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoN(mSegmentoN: TSegmentoN);
begin
  // Pagamento de Tributos e Impostos sem código de barras
  with mSegmentoN do
  begin
    Inc(FQtdeRegistros);
    Inc(FQtdeRegistrosLote);
    Inc(FSequencialDoRegistroNoLote);

    GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
    GravarCampo(FQtdeLotes, 4, tcInt);
    GravarCampo('3', 1, tcStr);
    GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
    GravarCampo('N', 1, tcStr);
    GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
    GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
    GravarCampo(SeuNumero, 20, tcStr);
    GravarCampo(NossoNumero, 20, tcStr);
    GravarCampo(NomeContribuinte, 30, tcStr, True);
    GravarCampo(DataPagamento, 8, tcDat);
    GravarCampo(ValorPagamento, 15, tcDe2);
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoN1(I: Integer);
var
  J: Integer;
begin
  // Informações complementares para pagamento da GPS
  for J := 0 to PagFor.Lote.Items[I].SegmentoN1.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN1.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr_SegN(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo('17', 2, tcStr);
      GravarCampo(Competencia, 6, tcInt);
      GravarCampo(ValorTributo, 15, tcDe2);
      GravarCampo(ValorOutrasEntidades, 15, tcDe2);
      GravarCampo(AtualizacaoMonetaria, 15, tcDe2);
      GravarCampo(' ', 45, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N1');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoN2(I: Integer);
var
  J: Integer;
begin
  // Informações complementares para pagamento de DARF
  for J := 0 to PagFor.Lote.Items[I].SegmentoN2.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN2.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr_SegN(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo('16', 2, tcStr);
      GravarCampo(Periodo, 8, tcDat);
      GravarCampo(Referencia, 17, tcStrZero);
      GravarCampo(ValorPrincipal, 15, tcDe2);
      GravarCampo(Multa, 15, tcDe2);
      GravarCampo(Juros, 15, tcDe2);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(' ', 18, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N2');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoN3(I: Integer);
var
  J: Integer;
begin
  // Informações complementares para pagamento de DARF SIMPLES
  for J := 0 to PagFor.Lote.Items[I].SegmentoN3.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN3.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr_SegN(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo('18', 2, tcStr);
      GravarCampo(Periodo, 8, tcDat);
      GravarCampo(ReceitaBruta, 15, tcDe2);
      GravarCampo(Percentual, 7, tcDe2);
      GravarCampo(ValorPrincipal, 15, tcDe2);
      GravarCampo(Multa, 15, tcDe2);
      GravarCampo(Juros, 15, tcDe2);
      GravarCampo(' ', 21, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N3');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoN4(I: Integer);
var
  J: Integer;
begin
  // Informações complementares para pagamento de GARE-SP (ICMS/DR/ITCMD)
  for J := 0 to PagFor.Lote.Items[I].SegmentoN4.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN4.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr_SegN(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(TpIndTributoToStr(PagFor.Geral.idTributo), 2, tcStr);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(InscEst, 12, tcStrZero);
      GravarCampo(NumEtiqueta, 13, tcStrZero);
      GravarCampo(Referencia, 6, tcInt);
      GravarCampo(NumParcela, 13, tcStrZero);
      GravarCampo(ValorReceita, 15, tcDe2);
      GravarCampo(Multa, 14, tcDe2);
      GravarCampo(Juros, 14, tcDe2);
      GravarCampo(' ', 1, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N4');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoN567(I: Integer);
var
  J: Integer;
begin
  // N5 - Informações complementares para pagamento de IPVA
  // N6 - Informações complementares para pagamento de DPVAT
  // N7 - Informações complementares para pagamento de LICENCIAMENTO
  for J := 0 to PagFor.Lote.Items[I].SegmentoN567.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN567.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr_SegN(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(TpIndTributoToStr(PagFor.Geral.idTributo), 2, tcStr);
      GravarCampo(Exercicio, 4, tcInt);
      GravarCampo(Renavam, 9, tcStrZero);
      GravarCampo(Estado, 2, tcStr);
      GravarCampo(Municipio, 5, tcInt);
      GravarCampo(Placa, 7, tcStr);

      case PagFor.Geral.idTributo of
        itIPVA:
          begin
            GravarCampo(OpcaoPagamento, 1, tcStr);
            GravarCampo(NovoRenavam, 12, tcStrZero);
            GravarCampo(' ', 55, tcStr);
          end;

        itDPVAT:
          begin
            GravarCampo('5', 1, tcStr);
            GravarCampo(NovoRenavam, 12, tcStrZero);
            GravarCampo(' ', 55, tcStr);
          end;

        itLicenciamento:
          begin
            GravarCampo('5', 1, tcStr);
            GravarCampo(OpcaoRetirada, 1, tcStr);
            GravarCampo(NovoRenavam, 12, tcStrZero);
            GravarCampo(' ', 54, tcStr);
          end;
      else
        begin
          GravarCampo(' ', 1, tcStr);
          GravarCampo(NovoRenavam, 12, tcStrZero);
          GravarCampo(' ', 55, tcStr);
        end;
      end;

      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N567');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoN8(I: Integer);
var
  J: Integer;
begin
  // Informações complementares para pagamento de DARJ
  for J := 0 to PagFor.Lote.Items[I].SegmentoN8.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN8.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(FormatFloat('0000', Receita), 6, tcStr);
      GravarCampo(InscricaoToStr_SegN(TipoContribuinte), 2, tcStrZero);
      GravarCampo(idContribuinte, 14, tcStrZero);
      GravarCampo(InscEst, 8, tcStrZero);
      GravarCampo(Origem, 16, tcStrZero);
      GravarCampo(ValorPrincipal, 15, tcDe2);
      GravarCampo(AtualizacaoMonetaria, 15, tcDe2);
      GravarCampo(Mora, 15, tcDe2);
      GravarCampo(Multa, 15, tcDe2);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(PeriodoParcela, 6, tcInt);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N8');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoN9(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoN9.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoN9.Items[J] do
    begin
      GeraSegmentoN(SegmentoN);

      GravarCampo(' ', 120, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('N9');
      IncluirLinha;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoO(I: Integer);
var
  J: Integer;
begin
  for J := 0 to PagFor.Lote.Items[I].SegmentoO.Count - 1 do
  begin
    FpLinha := '';

    with PagFor.Lote.Items[I].SegmentoO.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('O', 1, tcStr);
      GravarCampo(TpMovimentoToStr(TipoMovimento), 1, tcStr);
      GravarCampo(InMovimentoToStr(CodMovimento), 2, tcStr);
      GravarCampo(CodigoBarras, 44, tcStr);
      GravarCampo(NomeConcessionaria, 30, tcStr, True);
      GravarCampo(DataVencimento, 8, tcDat);
      GravarCampo(DataPagamento, 8, tcDat);
      GravarCampo(ValorPagamento, 15, tcDe2);
      GravarCampo(SeuNumero, 20, tcStr);
      GravarCampo(NossoNumero, 20, tcStr);
      GravarCampo(' ', 68, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('O');
      IncluirLinha;

      {opcionais segmento O}
      GeraSegmentoW(SegmentoW);
      GeraSegmentoZ(SegmentoZ);
      GeraSegmentoB(SegmentoB);
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoW(mSegmentoWList: TSegmentoWList);
var
  J: Integer;
begin
  for J := 0 to mSegmentoWList.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoWList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('W', 1, tcStr);
      GravarCampo(ComplementoRegistro, 1, tcInt);
      GravarCampo(Informacoes1ou2, 1, tcStr);
      GravarCampo(Informacoes1, 80, tcStr, True);
      GravarCampo(Informacoes2, 80, tcStr, True);

      if PagFGTS then // Se True Pagamento de FGTS
      begin
        GravarCampo('01', 2, tcStr);
        GravarCampo(FormatFloat('0000', StrToInt(CodReceita)), 6, tcStr);
        GravarCampo(TipoIdContribuinte, 2, tcStr);
        GravarCampo(idContribuinte, 14, tcStrZero);
        GravarCampo(Identificador, 16, tcStr);
        GravarCampo(LacreConecSocial, 9, tcStr);
        GravarCampo(LacreDV, 2, tcStr);
        GravarCampo(' ', 1, tcStr);
      end
      else
        GravarCampo(' ', 50, tcStr);

      GravarCampo(' ', 2, tcStr);
      GravarCampo(' ', 10, tcStr);

      ValidarLinha('W');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_CNAB240.GeraSegmentoZ(mSegmentoZList: TSegmentoZList);
var
  J: Integer;
begin
  for J := 0 to mSegmentoZList.Count - 1 do
  begin
    FpLinha := '';

    with mSegmentoZList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDoRegistroNoLote);

      GravarCampo(BancoToStr(PagFor.Geral.Banco), 3, tcStr);
      GravarCampo(FQtdeLotes, 4, tcInt);
      GravarCampo('3', 1, tcStr);
      GravarCampo(FSequencialDoRegistroNoLote, 5, tcInt);
      GravarCampo('Z', 1, tcStr);
      GravarCampo(Autenticacao, 64, tcStr);
      GravarCampo(SeuNumero, 25, tcStr);
      GravarCampo(' ', 137, tcStr);

      ValidarLinha('Z');
      IncluirLinha;
    end;
  end;
end;

procedure TArquivoW_CNAB240.ValidarLinha(const Tipo: string);
var
  Titulo: string;
begin
  if CharInSet(Tipo[1], ['0', '1', '5', '9']) then
    Titulo := 'Registro ['+ Tipo
  else
    Titulo := 'Segmento ['+ Tipo;

  if Length(FpLinha) <> 240 then
    raise Exception.Create(#13 + Titulo + '] inválido!' + #13 +
     'Deve conter 240 posições.' + #13 +
     'Registro: [' + FpLinha + ']' + #13 +
     'possui ' + IntToStr(Length(FpLinha)) + ' posições.');
end;

procedure TArquivoW_CNAB240.IncluirLinha;
begin
  if FArquivoTXT = '' then
    FArquivoTXT := FpLinha
  else
    FArquivoTXT := FArquivoTXT + sLineBreak + FpLinha;
end;

procedure TArquivoW_CNAB240.LimparRegistros;
begin
  // Falta Implementar
end;

procedure TArquivoW_CNAB240.UsarDadosConfiguracaoRegistro0;
begin
  with FpAOwner.ConfigGeral do
  begin
    if UsarDadosConfig then
    begin
      PagFor.Registro0.Empresa.Inscricao.Tipo := Empresa.TipoInscricao;
      PagFor.Registro0.Empresa.Inscricao.Numero := Empresa.NumeroInscricao;
      PagFor.Registro0.Empresa.Nome := Empresa.Nome;
      PagFor.Registro0.Empresa.Convenio := Empresa.Convenio;

      PagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo := Empresa.Conta.AgenciaCodigo;
      PagFor.Registro0.Empresa.ContaCorrente.Agencia.DV := Empresa.Conta.AgenciaDV;
      PagFor.Registro0.Empresa.ContaCorrente.Conta.TipoConta := Empresa.Conta.TipoConta;
      PagFor.Registro0.Empresa.ContaCorrente.Conta.Numero := Empresa.Conta.ContaNumero;
      PagFor.Registro0.Empresa.ContaCorrente.Conta.DV := Empresa.Conta.ContaDV;
      PagFor.Registro0.Empresa.ContaCorrente.DV := Empresa.Conta.DV;
    end;
  end;
end;

procedure TArquivoW_CNAB240.UsarDadosConfiguracaoRegistro1(I: Integer);
begin
  with FpAOwner.ConfigGeral do
  begin
    if UsarDadosConfig then
    begin
      PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Tipo := Empresa.TipoInscricao;
      PagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Numero := Empresa.NumeroInscricao;
      PagFor.Lote.Items[I].Registro1.Empresa.Nome := Empresa.Nome;
      PagFor.Lote.Items[I].Registro1.Empresa.Convenio := Empresa.Convenio;

      PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo := Empresa.Conta.AgenciaCodigo;
      PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.DV := Empresa.Conta.AgenciaDV;
      PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.TipoConta := Empresa.Conta.TipoConta;
      PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero := Empresa.Conta.ContaNumero;
      PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV := Empresa.Conta.ContaDV;
      PagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.DV := Empresa.Conta.DV;

      PagFor.Lote.Items[I].Registro1.Endereco.Logradouro := Empresa.Endereco.Logradouro;
      PagFor.Lote.Items[I].Registro1.Endereco.Numero := Empresa.Endereco.Numero;
      PagFor.Lote.Items[I].Registro1.Endereco.Complemento := Empresa.Endereco.Complemento;
      PagFor.Lote.Items[I].Registro1.Endereco.Cidade := Empresa.Endereco.Cidade;
      PagFor.Lote.Items[I].Registro1.Endereco.CEP := Empresa.Endereco.CEP;
      PagFor.Lote.Items[I].Registro1.Endereco.Estado := Empresa.Endereco.Estado;
    end;
  end;
end;

procedure TArquivoW_CNAB240.GerarLote(I: Integer);
begin
  try
    UsarDadosConfiguracaoRegistro1(I);

    GeraRegistro1(I);

    GeraSegmentoA(I);

    GeraSegmentoJ(I);

    GeraSegmentoN1(I);
    GeraSegmentoN2(I);
    GeraSegmentoN3(I);
    GeraSegmentoN4(I);
    GeraSegmentoN567(I);
    GeraSegmentoN8(I);
    GeraSegmentoN9(I);

    GeraSegmentoO(I);

    GeraRegistro5(I);

    LimparRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create('Não Foi Possível incluir Registros no Arquivo: ' +
              FNomeArquivo + #13 + E.Message);
    end;
  end;
end;

function TArquivoW_CNAB240.GerarTxt: Boolean;
var
  I: Integer;
begin
  FArquivoTXT := '';
  FConteudoTxt.Clear;

  UsarDadosConfiguracaoRegistro0;

  GeraRegistro0;

  for I := 0 to PagFor.Lote.Count - 1 do
    GerarLote(I);

  GeraRegistro9;

  FConteudoTxt.Text := FArquivoTXT;
  Result := True;
end;

end.
