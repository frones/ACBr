{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrPagForGravarTxt;

interface

uses
  SysUtils, Classes,
  ACBrPagForClass, ACBrPagForConversao, ACBrUtil;

type
  TPagForW = class(TPersistent)
  private
    FPagFor: TPagFor;

    FNomeArquivo: String;
    FArquivoTXT: String;

    FAtivo: Boolean;
    FQtdeLotes: Integer;
    FQtdeRegistros: Integer;
    FQtdeRegistrosLote: Integer;
    FSequencialDeLote: Integer;

    FVersaoLayout: TVersaoLayout;
    FveRegistro0: String;
    FveRegistro1: String;

    procedure GeraRegistro0;             // Registro Header de Arquivo
    procedure GeraRegistro1(I: Integer); // Registro Header de Lote
    
	procedure GeraSegmentoA(I: Integer);                     // Registro para Pagamentos (Pagamento através de Crédito em Conta Corrente, Cheque, OP, DOC, Pagamento com Autenticação ou Pix)
    procedure GeraSegmentoB(mSegmentoBList: TSegmentoBList); // Registro para Pagamentos (Pagamento através de Crédito em Conta Corrente, Cheque, OP, DOC, Pagamento com Autenticação ou Pix)
    procedure GeraSegmentoC(mSegmentoCList: TSegmentoCList); // Registro para Pagamentos (Pagamento através de Crédito em Conta Corrente, Cheque, OP, DOC, Pagamento com Autenticação ou Pix)
	
    procedure GeraSegmentoD(mSegmentoDList: TSegmentoDList); // Registro para Custódia de Cheques
    
	procedure GeraSegmentoE(mSegmentoEList: TSegmentoEList); // Registro para Extrato (Extrato de Conta Corrente para Conciliação Bancária)
    procedure GeraSegmentoF(mSegmentoFList: TSegmentoFList); // Registro para Extrato (Extrato para Gestão de Caixa)

    procedure GeraSegmentoJ(I: Integer);                           // Registro para Pagamentos (Pagamento de Títulos de Cobrança e QRCode Pix)
    procedure GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List); // Registro para Pagamentos (Pagamento de Títulos de Cobrança e QRCode Pix)
	
	// Registros de Informações complementares do Segmento N (Registro de Pagamento de Tributos e Impostos sem Código de Barras)
    procedure GeraSegmentoN1(I: Integer);
    procedure GeraSegmentoN2(I: Integer);
    procedure GeraSegmentoN3(I: Integer);
    procedure GeraSegmentoN4(I: Integer);
    procedure GeraSegmentoN567(I: Integer);
    procedure GeraSegmentoN8(I: Integer);
    procedure GeraSegmentoN9(I: Integer);
	
    procedure GeraSegmentoO(I: Integer); // Registro para Pagamentos (Pagamentos de Tributos)
	
    procedure GeraSegmentoP(I: Integer); // Registro para Cobrança (Títulos em Cobrança)
    procedure GeraSegmentoQ(I: Integer); // Registro para Cobrança (Títulos em Cobrança)
    procedure GeraSegmentoR(I: Integer); // Registro para Cobrança (Títulos em Cobrança)
    procedure GeraSegmentoS(I: Integer); // Registro para Cobrança (Títulos em Cobrança)

    procedure GeraSegmentoW(mSegmentoWList: TSegmentoWList); // Registro para Pagamentos (Pagamentos de Tributos)

    procedure GeraSegmentoY(I: Integer); // Registro para Cobrança (Títulos em Cobrança)

    procedure GeraSegmentoZ(mSegmentoZList: TSegmentoZList); // Registro para Pagamentos (Pagamentos de Tributos)

    procedure GeraRegistro5(I: Integer); // Registro Trailer de Lote
    procedure GeraRegistro9;             // Registro Trailer de Arquivo

    procedure WriteRecord(const Rec: String);
  public
    constructor Create(AOwner: TPagFor);
    destructor Destroy; override;
    function GerarTXT: String;
    function ObterNomeArquivo: string;

    procedure LimparRegistros;
    procedure GerarLote(I: Integer);
  published
    property PagFor: TPagFor read FPagFor write FPagFor;
    property NomeArquivo: string read FNomeArquivo write FNomeArquivo;
    property ArquivoTXT: string read FArquivoTXT write FArquivoTXT;
    property VersaoLayout: TVersaoLayout read FVersaoLayout write FVersaoLayout default ve084;
  end;

implementation

{ TPagForW }

constructor TPagForW.Create(AOwner: TPagFor);
begin
  inherited Create;
  FPagFor := AOwner;
  FArquivoTXT := '';
  FAtivo := True;
end;

destructor TPagForW.Destroy;
begin
  FAtivo := False;
  LimparRegistros;

  inherited Destroy;
end;

procedure TPagForW.GeraRegistro0;
var
  wregistro: string;
begin
  FPagFor.Registro0.Aviso.Clear;
  FQtdeRegistros := 1;
  FQtdeLotes := 0;

  wregistro := BancoToStr(FPagFor.Geral.Banco);
  wregistro := wregistro + '0000';
  wregistro := wregistro + '0';

  case FPagFor.Geral.Banco of
    pagItau:
      begin
        wregistro := wregistro + Space(6);
        wregistro := wregistro + '081';
      end;

    pagHSBC, pagSantander, pagSicred:
      wregistro := wregistro + Space(9);
  else
    wregistro := wregistro + Space(9);
  end;

  wregistro := wregistro + TpInscricaoToStr(FPagFor.Registro0.Empresa.Inscricao.Tipo);
  wregistro := wregistro + TBStrZero(FPagFor.Registro0.Empresa.Inscricao.Numero, 14);

  case FPagFor.Geral.Banco of
    pagSantander:
      begin
        wregistro := wregistro + TBStrZero(BancoToStr(FPagFor.Geral.Banco),4) +
                                 FormatFloat('0000', FPagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo) +
                                 TBStrZero(FPagFor.Registro0.Empresa.Convenio, 12);
        wregistro := wregistro + FormatFloat('00000', FPagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo);
        wregistro := wregistro + FormatFloat('0', FPagFor.Registro0.Empresa.ContaCorrente.Conta.TipoConta);
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Registro0.Empresa.ContaCorrente.Conta.Numero);
        wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1);
        wregistro := wregistro + ' ';
      end;

    pagItau:
      begin
        wregistro := wregistro + Space(20);
        wregistro := wregistro + FormatFloat('00000', FPagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo);
        wregistro := wregistro + ' ';
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Registro0.Empresa.ContaCorrente.Conta.Numero);
        wregistro := wregistro + ' ';

        if Trim(FPagFor.Registro0.Empresa.ContaCorrente.DV) <> '' then
          wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.DV, 1)
        else
          wregistro := wregistro + '0';
      end;

    pagHSBC:
      begin
        wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.Convenio, 20);
        wregistro := wregistro + FormatFloat('00000', FPagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo); // Cód. Ag. Agência da Conta   5
        wregistro := wregistro + ' '; // Filler Branco               1
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Registro0.Empresa.ContaCorrente.Conta.Numero); // Nr. Conta Corrente         12
        wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1); // Díg. Verif. Conta           1
        wregistro := wregistro + ' '; // Filler Branco               1
      end;

    pagSicred, pagBancoCECRED:
      begin
        wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.Convenio, 20);
        wregistro := wregistro + FormatFloat('00000', FPagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo); // Cód. Ag. Agência da Conta   5
        wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Agencia.DV, 1); // Díg. Verif. Conta           1
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Registro0.Empresa.ContaCorrente.Conta.Numero); // Nr. Conta Corrente         12
        wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1); // Díg. Verif. Conta           1
        wregistro := wregistro + ' '; // Filler Branco               1
      end;
    pagBancoDoBrasil:
      begin
        wregistro := wregistro + TBStrZero(FPagFor.Registro0.Empresa.Convenio, 9);
        wregistro := wregistro + '0126';
        wregistro := wregistro + Space(5);
        if FPagFor.Registro0.ReservadoEmpresa = 'T' then
          wregistro := wregistro + 'TS' //arquivo de teste
        else
          wregistro := wregistro + Space(2);
        wregistro := wregistro + FormatFloat('00000', FPagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo);
        wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Agencia.DV, 1);
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Registro0.Empresa.ContaCorrente.Conta.Numero);
        wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1);
        wregistro := wregistro + '0';
      end;
  else
    begin
      wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.Convenio, 20);
      wregistro := wregistro + FormatFloat('00000', FPagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo);
      wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Agencia.DV, 1);
      wregistro := wregistro + FormatFloat('000000', FPagFor.Registro0.Empresa.ContaCorrente.Conta.TipoConta);
      wregistro := wregistro + FormatFloat('000000', FPagFor.Registro0.Empresa.ContaCorrente.Conta.Numero);
      wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1);
      wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.DV, 1);
    end;
  end;

  wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Registro0.Empresa.Nome), 30);
  wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Registro0.NomeBanco), 30);
  wregistro := wregistro + Space(10);
  wregistro := wregistro + TpArquivoToStr(FPagFor.Registro0.Arquivo.Codigo);

  if FPagFor.Registro0.Arquivo.DataGeracao = 0 then
    wregistro := wregistro + '00000000'
  else
    wregistro := wregistro + FormatDateTime('ddmmyyyy', FPagFor.Registro0.Arquivo.DataGeracao);

  if FPagFor.Registro0.Arquivo.HoraGeracao = 0 then
    wregistro := wregistro + '000000'
  else
    wregistro := wregistro + FormatDateTime('hhmmss', FPagFor.Registro0.Arquivo.HoraGeracao);

  case FPagFor.Geral.Banco of

   pagBancoDoBrasil:
      begin
        wregistro := wregistro + FormatFloat('000000', FPagFor.Registro0.Arquivo.Sequencia);
        FveRegistro0 := '003';
      end;

    pagSicred:
      begin
        wregistro := wregistro + FormatFloat('000000', FPagFor.Registro0.Arquivo.Sequencia);
        FveRegistro0 := '084';
      end;

    pagSantander:
      begin
        wregistro := wregistro + FormatFloat('000000', FPagFor.Registro0.Arquivo.Sequencia);
        FveRegistro0 := '060';
      end;

    pagHSBC:
      begin
        wregistro := wregistro + FormatFloat('000000', FPagFor.Registro0.Arquivo.Sequencia);
        FveRegistro0 := '020'; // HSBC ’020” ou  “030”
      end;

    pagItau:
      begin
        wregistro := wregistro + '000000'; // Manual do itau cita (9) zeros não tendo a versão aqui
        FveRegistro0 := '000';
      end;

    pagBradesco:
      begin
        wregistro := wregistro + FormatFloat('000000', FPagFor.Registro0.Arquivo.Sequencia);
        FveRegistro0 := '089';
      end;

    pagBancoCECRED:
      begin
        wregistro := wregistro + FormatFloat('000000', FPagFor.Registro0.Arquivo.Sequencia);
        FveRegistro0 := '088';
      end;

    pagBancoSafra:
      begin
        wregistro := wregistro + FormatFloat('000000', FPagFor.Registro0.Arquivo.Sequencia);
        FveRegistro0 := '103';
      end;

  else
    begin
      wregistro := wregistro + '000000';
      FveRegistro0 := '000';
    end;
  end;

  wregistro := wregistro + FveRegistro0;
  wregistro := wregistro + FormatFloat('00000', FPagFor.Registro0.Arquivo.Densidade);

  case FPagFor.Geral.Banco of
    pagBancoDoBrasil:
      begin
        wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Registro0.ReservadoBanco), 20);
        wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Registro0.ReservadoEmpresa), 20);
        wregistro := wregistro + Space(11);
        wregistro := wregistro + Space(3);
        wregistro := wregistro + '000'; // Uso exclusivo das vans
        wregistro := wregistro + '00'; // Codigo do Servico
        wregistro := wregistro + '0000000000';
      end;

    pagSantander, pagSicred:
      begin
        wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Registro0.ReservadoBanco), 20);
        wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Registro0.ReservadoEmpresa), 20);
        wregistro := wregistro + Space(29);
      end;

    pagHSBC:
      begin
        wregistro := wregistro + 'CPG'; // Fixo "CPG"
        wregistro := wregistro + 'Y2K'; // Uso exclusivo das vans
        wregistro := wregistro + Space(14); // Codigo do Servico
        wregistro := wregistro + Space(49);
      end;

    pagItau:
      begin
        wregistro := wregistro + Space(69);
      end;
  else
    begin
      wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Registro0.ReservadoBanco), 20);
      wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Registro0.ReservadoEmpresa), 20);
      wregistro := wregistro + Space(29);
    end;
  end;

  WriteRecord(wregistro);
  FArquivoTXT := wregistro;
end;

procedure TPagForW.GeraRegistro1(I: Integer);
var
  wregistro: string;
begin
  Inc(FQtdeRegistros);
  Inc(FQtdeLotes);

  if (FPagFor.Geral.Banco = pagBancoDoBrasil) then
      FQtdeRegistrosLote := 0
  else
    FQtdeRegistrosLote := 1;
  FSequencialDeLote  := 0;

  case FPagFor.Lote.Items[I].Registro1.Servico.TipoServico of
    tsGestaoCaixa, tsCustodiaCheques, tsAlegacaoSacado, tsPagamentoContas, tsCompror, tsComprorRotativo:
      FveRegistro1 := '010';

    tsVendor:
      FveRegistro1 := '012';

    tsConsignacaoParcelas:
      FveRegistro1 := '020';

    tsBloquetoEletronico:
      FveRegistro1 := '022';

    tsDebitos, tsPagamentoFornecedor:
      FveRegistro1 := '030';

    tsConciliacaoBancaria:
      FveRegistro1 := '033';
  else
    FveRegistro1 := '043';
  end;

  case FPagFor.Geral.Banco of    //Não ultilizei o codigo acima a principio não é pelo tipo de serviço
    pagSantander:
      begin
        case FPagFor.Lote.Items[I].Registro1.Servico.FormaLancamento of
          flLiquidacaoTitulosProprioBanco, flLiquidacaoTitulosOutrosBancos,
          flPagamentoConcessionarias, flPagamentoContas, flTributoDARFNormal,
          flTributoGPS, flTributoDARFSimples, flTributoIPTU,
          flTributoDARJ, flTributoGARESPICMS, flTributoGARESPDR,
          flTributoGARESPITCMD, flTributoIPVA,
          flTributoLicenciamento, flTributoDPVAT, flTributoGNRe :
            FveRegistro1 := '010';   //Pagamento com codigo de barras  e Convenios  SEGMENTO 'J' e 'O'.
        else
          FveRegistro1 := '031';  //Pagamento DOC / TED
        end;
      end;

    pagSicred:
      FveRegistro1 := '042';

    pagHSBC:
      FveRegistro1 := '020';

    pagBancoDoBrasil:
      FveRegistro1 := '003';

    pagItau:
      begin
        if FPagFor.Lote.Items[I].SegmentoA.Count > 0 then
          // Se for parte do Header (Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente)
          // Segmento A - Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente
          // Segmento A - Pagamentos através de Nota Fiscal – Liquidação Eletrônica
          FveRegistro1 := '040'
        else
          FveRegistro1 := '030'
      end;
    pagBradesco:
      FveRegistro1 := '040';
  else
    FveRegistro1 := '000'
  end;

  wregistro := BancoToStr(FPagFor.Geral.Banco);
  wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
  wregistro := wregistro + '1';
  wregistro := wregistro + TpOperacaoToStr(FPagFor.Lote.Items[I].Registro1.Servico.Operacao);
  wregistro := wregistro + TpServicoToStr(FPagFor.Lote.Items[I].Registro1.Servico.TipoServico);
  wregistro := wregistro + FmLancamentoToStr(FPagFor.Lote.Items[I].Registro1.Servico.FormaLancamento);
  wregistro := wregistro + FveRegistro1;
  wregistro := wregistro + ' ';
  wregistro := wregistro + TpInscricaoToStr(FPagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Tipo);
  wregistro := wregistro + Copy(TBStrZero(FPagFor.Lote.Items[I].Registro1.Empresa.Inscricao.Numero, 15), 2, 14);

  case FPagFor.Geral.Banco of
    pagSicred, pagBancoCECRED:
      begin
        wregistro := wregistro + PadRight(FPagFor.Lote.Items[I].Registro1.Empresa.Convenio, 20);
        wregistro := wregistro + FormatFloat('00000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo);
        wregistro := wregistro + PadRight(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.DV,1);
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero);
        wregistro := wregistro + PadRight(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV, 1);
        wregistro := wregistro + ' ';
      end;

    pagSantander:
      begin
        wregistro := wregistro + TBStrZero(BancoToStr(FPagFor.Geral.Banco),4) +
                                 FormatFloat('0000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo) +
                                 TBStrZero(FPagFor.Lote.Items[I].Registro1.Empresa.Convenio, 12);
        wregistro := wregistro + FormatFloat('00000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo);
        wregistro := wregistro + FormatFloat('0', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.TipoConta);
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero);
        wregistro := wregistro + PadRight(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV, 1);
        wregistro := wregistro + ' ';
      end;

    pagItau:
      begin
        wregistro := wregistro + Space(20);
        wregistro := wregistro + FormatFloat('00000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo);
        wregistro := wregistro + ' '; // Filler Branco               1
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Registro0.Empresa.ContaCorrente.Conta.Numero); // Nr. Conta Corrente         12
        wregistro := wregistro + ' ';

        if PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV), 1) <> ' ' then
          wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV), 1)
        else
          wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.DV), 1);
      end;

    pagHSBC:
      begin
        wregistro := wregistro + PadRight(FPagFor.Lote.Items[I].Registro1.Empresa.Convenio, 20);
        wregistro := wregistro + FormatFloat('00000', FPagFor.Registro0.Empresa.ContaCorrente.Agencia.Codigo); // Cód. Ag. Agência da Conta   5
        wregistro := wregistro + ' '; // Filler Branco               1
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Registro0.Empresa.ContaCorrente.Conta.Numero); // Nr. Conta Corrente         12
        wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1); // Díg. Verif. Conta           1
        wregistro := wregistro + ' '; // Filler Branco               1
      end;
    pagBancoDoBrasil:
      begin
        wregistro := wregistro + TBStrZero(FPagFor.Registro0.Empresa.Convenio, 9);
        wregistro := wregistro + '0126';
        wregistro := wregistro + Space(5);
        if FPagFor.Registro0.ReservadoEmpresa = 'T' then
          wregistro := wregistro + 'TS' //arquivo de teste
        else
          wregistro := wregistro + Space(2);

        wregistro := wregistro + FormatFloat('00000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo);
        wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.DV), 1);
        wregistro := wregistro + FormatFloat('000000000000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero);
        wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV), 1);
        wregistro := wregistro + '0';
      end;
  else
    begin
      wregistro := wregistro + PadRight(FPagFor.Lote.Items[I].Registro1.Empresa.Convenio, 20);
      wregistro := wregistro + FormatFloat('00000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.Codigo);
      wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Agencia.DV), 1);
      wregistro := wregistro + FormatFloat('000000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.TipoConta);
      wregistro := wregistro + FormatFloat('000000', FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.Numero);
      wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.Conta.DV), 1);
      wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Empresa.ContaCorrente.DV), 1);
    end;
  end;

  wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Empresa.Nome), 30);
  wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Informacao1), 40);
  wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Endereco.Logradouro), 30);
  wregistro := wregistro + FormatFloat('00000', FPagFor.Lote.Items[I].Registro1.Endereco.Numero);
  wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Endereco.Complemento), 15);
  wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Endereco.Cidade), 20);

  case FPagFor.Geral.Banco of
    pagBancoDoBrasil:
      begin
        wregistro := wregistro + TBStrZero(Copy(IntToStr(FPagFor.Lote.Items[I].Registro1.Endereco.CEP), 1, 5), 5);
        wregistro := wregistro + PadRight(Copy(IntToStr(FPagFor.Lote.Items[I].Registro1.Endereco.CEP),6, 3), 3);
      end;
    else
      wregistro := wregistro + FormatFloat('00000000', FPagFor.Lote.Items[I].Registro1.Endereco.CEP);
  end;
  wregistro := wregistro + PadRight(TiraAcentos(FPagFor.Lote.Items[I].Registro1.Endereco.Estado), 2);

  case FPagFor.Geral.Banco of
    pagHSBC:
      begin
        wregistro := wregistro + 'S'; // Comprovante de Pagamento Emissão em Lote  “S=Sim” ou “N=Não”
        wregistro := wregistro + Space(17);
      end;
    pagBancoDoBrasil:
      begin
        wregistro := wregistro + Space(8);
        wregistro := wregistro + '0000000000';
      end;
    else
      begin
        wregistro := wregistro + Space(8);
        wregistro := wregistro + Space(10);
      end;
  end;

  WriteRecord(wregistro);
  FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
end;

procedure TPagForW.GeraRegistro5(I: Integer);
var
  wregistro: string;
begin
  Inc(FQtdeRegistros);
  Inc(FQtdeRegistrosLote);

  wregistro := BancoToStr(FPagFor.Geral.Banco);
  wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
  wregistro := wregistro + '5';
  wregistro := wregistro + Space(9);

  case FPagFor.Geral.Banco of
    pagSantander, pagSicred, pagBancoCECRED, pagBancoSafra:
      begin
        wregistro := wregistro + FormatFloat('000000', FQtdeRegistrosLote);
        wregistro := wregistro + FormatFloat('000000000000000000', FPagFor.Lote.Items[I].Registro5.Valor * 100);
        wregistro := wregistro + '000000000000000000';
        wregistro := wregistro + '000000';
        wregistro := wregistro + Space(165);
        wregistro := wregistro + Space(10);
      end;

    pagHSBC:
      begin
        wregistro := wregistro + FormatFloat('000000', FQtdeRegistrosLote);
        wregistro := wregistro + Space(3);
        wregistro := wregistro + FormatFloat('000000000000000', FPagFor.Lote.Items[I].Registro5.Valor * 100);
        wregistro := wregistro + Space(199);
      end;

    pagItau:
      begin
        wregistro := wregistro + FormatFloat('000000', FQtdeRegistrosLote);

        if (FPagFor.Lote.Items[I].Registro1.Servico.FormaLancamento = flPagamentoConcessionarias) then
        begin // Contas de Concessionárias e Tributos com código de barras
          wregistro := wregistro + FormatFloat('000000000000000000', FPagFor.Lote.Items[I].Registro5.Valor * 100);
          wregistro := wregistro + FormatFloat('000000000000000', FPagFor.Lote.Items[I].Registro5.QtdeMoeda * 100000000); // 8 casas decimais
          wregistro := wregistro + Space(174);
          wregistro := wregistro + Space(10);
        end
        else
        if FPagFor.Lote.Items[I].Registro1.Servico.TipoServico = tsPagamentoSalarios then
        begin // fgts
          wregistro := wregistro + FormatFloat('00000000000000', FPagFor.Lote.Items[I].Registro5.Valor * 100);
          wregistro := wregistro + FormatFloat('00000000000000', FPagFor.Lote.Items[I].Registro5.TotalOutrasEntidades * 100);
          wregistro := wregistro + FormatFloat('00000000000000', FPagFor.Lote.Items[I].Registro5.TotalValorAcrescimo * 100);
          wregistro := wregistro + FormatFloat('00000000000000', FPagFor.Lote.Items[I].Registro5.TotalValorArrecadado * 100);
          wregistro := wregistro + Space(151);
          wregistro := wregistro + Space(10);
        end
        else
        begin
          // Pagamentos através de cheque, OP, DOC, TED e crédito em conta corrente
          // Liquidação de títulos (bloquetos) em cobrança no Itaú e em outros Bancos
          if PadRight(FPagFor.Lote.Items[I].Registro1.Informacao1, 2) = '06' then {informe de rendimentos}
            wregistro := wregistro + '000000000000000000' // informe de rendimento, este valor é 0
          else
            wregistro := wregistro + FormatFloat('000000000000000000', FPagFor.Lote.Items[I].Registro5.Valor * 100);

          wregistro := wregistro + '000000000000000000';
          wregistro := wregistro + Space(171);
          wregistro := wregistro + Space(10);
        end;
      end;
    pagBradesco:
      begin
        wregistro := wregistro + FormatFloat('000000', FQtdeRegistrosLote);
        if (FPagFor.Lote.Items[I].Registro1.Servico.FormaLancamento in [flLiquidacaoTitulosProprioBanco,flLiquidacaoTitulosOutrosBancos] ) then
        begin
          wregistro := wregistro + FormatFloat('000000000000000000', FPagFor.Lote.Items[I].Registro5.Valor * 100);
          wregistro := wregistro + FormatFloat('000000000000000000', FPagFor.Lote.Items[I].Registro5.QtdeMoeda * 100000); // 5 casas decimais
          wregistro := wregistro + '000000';
          wregistro := wregistro + Space(165);
          wregistro := wregistro + Space(10);
        end;
      end;
    pagBancoDoBrasil:
      begin
        Inc(FQtdeRegistrosLote);
        wregistro := wregistro + FormatFloat('000000', FQtdeRegistrosLote);  //Somo + 1 pois iniciei com 0 no regstro1
        wregistro := wregistro + FormatFloat('000000000000000000', FPagFor.Lote.Items[I].Registro5.Valor * 100);
        wregistro := wregistro + '000000000000000000';
        wregistro := wregistro + '000000';
        wregistro := wregistro + Space(165);
        wregistro := wregistro + '0000000000';
      end;
  else
    begin
      wregistro := wregistro + FormatFloat('000000', FQtdeRegistrosLote);
      wregistro := wregistro + FormatFloat('000000', FPagFor.Lote.Items[I].Registro5.TotalCobrancaSimples.QtdeTitulosCobranca);
      wregistro := wregistro + FormatFloat('00000000000000000', FPagFor.Lote.Items[I].Registro5.TotalCobrancaSimples.ValorTitulosCarteira * 100);
      wregistro := wregistro + FormatFloat('000000', FPagFor.Lote.Items[I].Registro5.TotalCobrancaVinculada.QtdeTitulosCobranca);
      wregistro := wregistro + FormatFloat('00000000000000000', FPagFor.Lote.Items[I].Registro5.TotalCobrancaVinculada.ValorTitulosCarteira * 100);
      wregistro := wregistro + FormatFloat('000000', FPagFor.Lote.Items[I].Registro5.TotalCobrancaCaucionada.QtdeTitulosCobranca);
      wregistro := wregistro + FormatFloat('00000000000000000', FPagFor.Lote.Items[I].Registro5.TotalCobrancaCaucionada.ValorTitulosCarteira * 100);
      wregistro := wregistro + FormatFloat('000000', FPagFor.Lote.Items[I].Registro5.TotalCobrancaDescontada.QtdeTitulosCobranca);
      wregistro := wregistro + FormatFloat('00000000000000000', FPagFor.Lote.Items[I].Registro5.TotalCobrancaDescontada.ValorTitulosCarteira * 100);
      wregistro := wregistro + FormatFloat('000000', FPagFor.Lote.Items[I].Registro5.NumAvisoLancamento);
      wregistro := wregistro + Space(2);
      wregistro := wregistro + Space(117);
    end;
  end;

  WriteRecord(wregistro);
  FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
end;

procedure TPagForW.GeraRegistro9;
var
  wregistro: string;
begin
  Inc(FQtdeRegistros);

  wregistro := BancoToStr(FPagFor.Geral.Banco);
  wregistro := wregistro + '9999';
  wregistro := wregistro + '9';
  wregistro := wregistro + Space(9);
  wregistro := wregistro + FormatFloat('000000', FQtdeLotes);
  wregistro := wregistro + FormatFloat('000000', FQtdeRegistros);

  case FPagFor.Geral.Banco of
    pagSicred, pagBancoCECRED, pagBancoSafra, pagBradesco, pagBancoDoBrasil:
      wregistro := wregistro + '000000';
  else
    wregistro := wregistro + Space(6);
  end;
  wregistro := wregistro + Space(205);

  WriteRecord(wregistro);
  FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
//  if FPagFor.Geral.Banco = pagItau then
//    FArquivoTXT := FArquivoTXT + sLineBreak; // Ao importar arquivo no site do itau, da  erro se não tiver esta quebra de linha final
end;

procedure TPagForW.GerarLote(I: Integer);
begin
  try
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
    GeraSegmentoP(I);
    GeraSegmentoQ(I);
    GeraSegmentoR(I);
    GeraSegmentoS(I);
    GeraSegmentoY(I);
    GeraRegistro5(I);

    LimparRegistros;
  except
    on E: Exception do
    begin
      raise Exception.Create('Não Foi Possível incluir Registros no Arquivo: ' + FNomeArquivo + #13 + E.Message);
    end;
  end;
end;

function TPagForW.GerarTXT: String;
var
  I: Integer;
begin
  case VersaoLayout of
    ve084:
      FveRegistro0 := '084';
  else
    FveRegistro0 := '000';
  end;

  GeraRegistro0;

  for I := 0 to FPagFor.Lote.Count - 1 do
    GerarLote(I);

  GeraRegistro9;
  Result := FArquivoTXT;
end;

procedure TPagForW.GeraSegmentoA(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoA.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoA.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'A';
      wregistro := wregistro + TpMovimentoToStr(TipoMovimento);
      wregistro := wregistro + InMovimentoToStr(CodMovimento);
      wregistro := wregistro + FormatFloat('000', Favorecido.Camara);
      wregistro := wregistro + BancoToStr(Favorecido.Banco);

      case FPagFor.Geral.Banco of
        pagSicred:
          begin
            wregistro := wregistro + FormatFloat('00000', Favorecido.ContaCorrente.Agencia.Codigo);
            wregistro := wregistro + PadRight(TiraAcentos(Favorecido.ContaCorrente.Agencia.DV), 1);
            wregistro := wregistro + FormatFloat('000000000000', Favorecido.ContaCorrente.Conta.Numero);
            wregistro := wregistro + PadRight(TiraAcentos(Favorecido.ContaCorrente.Conta.DV), 1);
            wregistro := wregistro + PadRight(TiraAcentos(Favorecido.ContaCorrente.DV), 1);
          end;

        pagHSBC, pagSantander:
          begin
            wregistro := wregistro + FormatFloat('00000', Favorecido.ContaCorrente.Agencia.Codigo); // Cód. Ag. Agência da Conta   5
            wregistro := wregistro + ' '; // Filler Branco               1
            wregistro := wregistro + FormatFloat('000000000000', Favorecido.ContaCorrente.Conta.Numero); // Nr. Conta Corrente         12
            wregistro := wregistro + PadRight(FPagFor.Registro0.Empresa.ContaCorrente.Conta.DV, 1); // Díg. Verif. Conta           1
            wregistro := wregistro + ' '; // Filler Branco               1
          end;

        pagItau:
          begin
            wregistro := wregistro + FormatFloat('00000', Favorecido.ContaCorrente.Agencia.Codigo);
            wregistro := wregistro + ' ';

            if (FPagFor.Lote.Items[I].Registro1.Servico.FormaLancamento = flChequePagamento) or
               (FPagFor.Lote.Items[I].Registro1.Servico.FormaLancamento = flOPDisposicao) then
              wregistro := wregistro + '000000000000 0' // Conta - digito são zerados
            else
            begin
              wregistro := wregistro + FormatFloat('000000000000', Favorecido.ContaCorrente.Conta.Numero);

              if (Trim(Favorecido.ContaCorrente.Agencia.DV) <> '0') and
                 (Trim(Favorecido.ContaCorrente.Conta.DV) <> '0') and
                 (Length(Trim(Favorecido.ContaCorrente.Agencia.DV) + Trim(Favorecido.ContaCorrente.Conta.DV)) > 1) then
                wregistro := wregistro + Trim(Favorecido.ContaCorrente.Agencia.DV) + Trim(Favorecido.ContaCorrente.Conta.DV)
              else
              begin
                wregistro := wregistro + Space(1);
                wregistro := wregistro + PadRight(Trim(Favorecido.ContaCorrente.Conta.DV), 1);
              end;
            end;
          end;
        pagBancoDoBrasil:
          begin
            wregistro := wregistro + FormatFloat('00000', Favorecido.ContaCorrente.Agencia.Codigo);
            wregistro := wregistro + PadRight(TiraAcentos(Favorecido.ContaCorrente.Agencia.DV), 1);
            wregistro := wregistro + FormatFloat('000000000000', Favorecido.ContaCorrente.Conta.Numero);
            wregistro := wregistro + PadRight(TiraAcentos(Favorecido.ContaCorrente.Conta.DV), 1);
            wregistro := wregistro + PadRight(TiraAcentos(Favorecido.ContaCorrente.DV), 1);
          end;
      else
        begin
          wregistro := wregistro + FormatFloat('00000', Favorecido.ContaCorrente.Agencia.Codigo);
          wregistro := wregistro + PadRight(TiraAcentos(Favorecido.ContaCorrente.Agencia.DV), 1);
          wregistro := wregistro + FormatFloat('000000', Favorecido.ContaCorrente.Conta.TipoConta);
          wregistro := wregistro + FormatFloat('000000', Favorecido.ContaCorrente.Conta.Numero);
          wregistro := wregistro + PadRight(TiraAcentos(Favorecido.ContaCorrente.Conta.DV), 1);
          wregistro := wregistro + PadRight(TiraAcentos(Favorecido.ContaCorrente.DV), 1);
        end;
      end;

      wregistro := wregistro + PadRight(TiraAcentos(Favorecido.Nome), 30);

      if FPagFor.Geral.Banco = pagBancoDoBrasil then
      begin
        wregistro := wregistro + PadRight(TiraAcentos(Credito.SeuNumero), 20);
      end
      else
      begin
        wregistro := wregistro + PadRight(TiraAcentos(Credito.SeuNumero), 16);
        wregistro := wregistro + Space(4);
      end;

      if Credito.DataPagamento = 0 then
        wregistro := wregistro + '00000000'
      else
        wregistro := wregistro + FormatDateTime('ddmmyyyy', Credito.DataPagamento);

      case FPagFor.Geral.Banco of
        pagBancoDoBrasil:
          begin
            wregistro := wregistro + TpMoedaToStr(Credito.Moeda.Tipo);
            wregistro := wregistro + '000000000000000';     //Zeros
            wregistro := wregistro + FormatFloat('000000000000000', Credito.ValorPagamento * 100);//“VALOR DO PAGTO” deve ser informado com zeros.
            wregistro := wregistro + PadRight(TiraAcentos(Credito.NossoNumero), 20);
            wregistro := wregistro + '00000000';
            wregistro := wregistro + '000000000000000';
            wregistro := wregistro + PadRight(TiraAcentos(Informacao2), 40);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoDOC), 2);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoTED), 5);
            wregistro := wregistro + Space(5);
            wregistro := wregistro + FormatFloat('0', Aviso);
            wregistro := wregistro + '0000000000';
        end;

        pagSantander:
          begin
            wregistro := wregistro + TpMoedaToStr(Credito.Moeda.Tipo);
            wregistro := wregistro + '000000000000000';     //Zeros
            wregistro := wregistro + FormatFloat('000000000000000', Credito.ValorPagamento * 100);//“VALOR DO PAGTO” deve ser informado com zeros.
            wregistro := wregistro + PadRight(TiraAcentos(Credito.NossoNumero), 20);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', Credito.DataReal);
            wregistro := wregistro + FormatFloat('000000000000000', Credito.ValorReal * 100);
            wregistro := wregistro + PadRight(TiraAcentos(Informacao2), 40);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoDOC), 2);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoTED), 5);
            wregistro := wregistro + Space(5);
            wregistro := wregistro + FormatFloat('0', Aviso);
            wregistro := wregistro + Space(10);
          end;

        pagSicred:
          begin
            wregistro := wregistro + TpMoedaToStr(Credito.Moeda.Tipo);
            wregistro := wregistro + '000000000000000';     //Zeros
            wregistro := wregistro + FormatFloat('000000000000000', Credito.ValorPagamento * 100);//“VALOR DO PAGTO” deve ser informado com zeros.
            wregistro := wregistro + PadRight(TiraAcentos(Credito.NossoNumero), 20);
            wregistro := wregistro + '00000000';
            wregistro := wregistro + FormatFloat('000000000000000', Credito.ValorReal * 100);
            wregistro := wregistro + PadRight(TiraAcentos(Informacao2), 40);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoDOC), 2);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoTED), 5);
            wregistro := wregistro + Space(5);
            wregistro := wregistro + FormatFloat('0', Aviso);
            wregistro := wregistro + Space(10);
          end;

        pagHSBC:
          begin
            wregistro := wregistro + 'R$ ';
            wregistro := wregistro + Space(17);
            wregistro := wregistro + FormatFloat('0000000000000', Credito.ValorPagamento * 100);
            wregistro := wregistro + 'N'; // Comprovante de Pagto. Emissão Individual “S=Sim ou N=Não”
            wregistro := wregistro + PadRight(TiraAcentos(''), 30); // ReferenciaSacado
            wregistro := wregistro + Space(12);
            wregistro := wregistro + PadRight(TiraAcentos(Informacao2), 40);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoDOC), 2);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoTED), 5);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoComp), 2);
            wregistro := wregistro + Space(3);
            wregistro := wregistro + FormatFloat('0', Aviso);
            wregistro := wregistro + Space(10);
          end;

        pagItau:
          begin
            wregistro := wregistro + 'REA';
            wregistro := wregistro + '00000000';     //CÓDIGO ISPB IDENTIFICAÇÃO DA INSTITUIÇÃO PARA O SPB
            wregistro := wregistro + '0000000';     // ZEROS COMPLEMENTO DE REGISTRO
            wregistro := wregistro + FormatFloat('000000000000000', Credito.ValorPagamento * 100);//“VALOR DO PAGTO” deve ser informado com zeros.
            wregistro := wregistro + PadRight(TiraAcentos(Credito.NossoNumero), 20);

            if Credito.DataReal > 0 then
              wregistro := wregistro + FormatDateTime('ddmmyyyy', Credito.DataReal)
            else
              wregistro := wregistro + '00000000';

            wregistro := wregistro + FormatFloat('000000000000000', Credito.ValorReal * 100);
            wregistro := wregistro + PadRight(TiraAcentos(Informacao2), 40);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoDOC), 2);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoTED), 5);
            wregistro := wregistro + Space(5);
            wregistro := wregistro + FormatFloat('0', Aviso);
            wregistro := wregistro + Space(10);
          end;
      else
        begin
          wregistro := wregistro + TpMoedaToStr(Credito.Moeda.Tipo);
          wregistro := wregistro + FormatFloat('000000000000000', Credito.Moeda.Qtde * 100000);
          wregistro := wregistro + FormatFloat('000000000000000', Credito.ValorPagamento * 100);
          wregistro := wregistro + PadRight(TiraAcentos(Credito.NossoNumero), 20);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', Credito.DataReal);
          wregistro := wregistro + FormatFloat('000000000000000', Credito.ValorReal * 100);
          wregistro := wregistro + PadRight(TiraAcentos(Informacao2), 40);
          wregistro := wregistro + PadRight(TiraAcentos(CodigoDOC), 2);
          wregistro := wregistro + PadRight(TiraAcentos(CodigoTED), 5);
          wregistro := wregistro + Space(3);
          wregistro := wregistro + PadRight(TiraAcentos(CodigoComp), 2);
          wregistro := wregistro + FormatFloat('0', Aviso);
          wregistro := wregistro + Space(10);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {opcionais do segmento A}
      GeraSegmentoB(SegmentoB);
      GeraSegmentoC(SegmentoC);
      GeraSegmentoD(SegmentoD);
      GeraSegmentoE(SegmentoE);
      GeraSegmentoF(SegmentoF);
      GeraSegmentoZ(SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoB(mSegmentoBList: TSegmentoBList);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to {FPagFor.Lote.Items[I].SegmentoB}mSegmentoBList.Count - 1 do
  begin
    with {FPagFor.Lote.Items[I].SegmentoB}mSegmentoBList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      if (FPagFor.Geral.Banco = PagSicred) or (FPagFor.Geral.Banco = pagBancoDoBrasil) then
        Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'B';
      if(Inscricao.PixTipoChave = '')then
      begin
        wregistro := wregistro + Space(3);
        wregistro := wregistro + TpInscricaoToStr(Inscricao.Tipo);
        wregistro := wregistro + TBStrZero(Inscricao.Numero, 14);
        wregistro := wregistro + PadRight(TiraAcentos(Endereco.Logradouro), 30);
        wregistro := wregistro + FormatFloat('00000', Endereco.Numero);
        wregistro := wregistro + PadRight(TiraAcentos(Endereco.Complemento), 15);
        wregistro := wregistro + PadRight(TiraAcentos(Endereco.Bairro), 15);
        wregistro := wregistro + PadRight(TiraAcentos(Endereco.Cidade), 20);
        wregistro := wregistro + FormatFloat('00000000', Endereco.CEP);
        wregistro := wregistro + PadRight(Endereco.Estado, 2);
      end;
      case FPagFor.Geral.Banco of
        pagSantander:
          begin
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
            wregistro := wregistro + FormatFloat('000000000000000', Valor * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Abatimento * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Desconto * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Mora * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Multa * 100);
            wregistro := wregistro + '0000';
            wregistro := wregistro + Space(11);
            wregistro := wregistro + FormatFloat('00000', CodigoUG);
            wregistro := wregistro + ' ';
            wregistro := wregistro + 'N';
            wregistro := wregistro + Space(8);
          end;

        pagHSBC:
          begin
            wregistro := wregistro + Space(105);

            if CodigoUG > 0 then
              wregistro := wregistro + PadRight(FormatFloat('0', CodigoUG), 8)
            else
              wregistro := wregistro + PadRight(TiraAcentos(CodigoDOC), 8); // Alfanumerico (Brancos)
          end;

        pagItau:
          begin
		    if(Inscricao.PixTipoChave <> '')then
            begin
              wregistro := wregistro + PadRight(Inscricao.PixTipoChave, 2);
              wregistro := wregistro + ' ';
              wregistro := wregistro + TpInscricaoToStr(Inscricao.Tipo);
              wregistro := wregistro + TBStrZero(Inscricao.Numero, 14);
              wregistro := wregistro + Space(30);
              wregistro := wregistro + PadRight(Inscricao.PixMensagem, 65);
            end;
            wregistro := wregistro + PadRight(Email, 100); //ENDEREÇO DE E-MAIL
            wregistro := wregistro + Space(3);
            wregistro := wregistro + Space(10);
          end;

        pagSicred:
          begin
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
            wregistro := wregistro + FormatFloat('000000000000000', Valor * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Abatimento * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Desconto * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Mora * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Multa * 100);
            wregistro := wregistro + PadRight(TiraAcentos(CodigoDOC), 15);
            wregistro := wregistro + FormatFloat('0', Aviso);
            wregistro := wregistro + FormatFloat('000000', CodigoUG);
            wregistro := wregistro + FormatFloat('00000000', CodigoISPB)
          end
      else
        begin
          wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
          wregistro := wregistro + FormatFloat('000000000000000', Valor * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Abatimento * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Desconto * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Mora * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Multa * 100);
          wregistro := wregistro + PadRight(TiraAcentos(CodigoDOC), 15);
          wregistro := wregistro + FormatFloat('0', Aviso);
          wregistro := wregistro + FormatFloat('000000', CodigoUG);
          wregistro := wregistro + Space(8);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoC(mSegmentoCList: TSegmentoCList);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to {FPagFor.Lote.Items[I].SegmentoC}mSegmentoCList.Count - 1 do
  begin
    with {FPagFor.Lote.Items[I].SegmentoC}mSegmentoCList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'C';

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            wregistro := wregistro + FormatFloat('000000000000000', ValorCSLL * 100);
            wregistro := wregistro + Space(8);

            if Vencimento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', Vencimento);

            wregistro := wregistro + FormatFloat('000000000000000', ValorDocumento * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorPIS * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorIR * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorISS * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorCOFINS * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Descontos * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Abatimentos * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Deducoes * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Mora * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Multa * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Acrescimos * 100);
            wregistro := wregistro + PadRight(NumeroFaturaDocumento, 20);
            wregistro := wregistro + Space(10);
          end;
        pagBancoDoBrasil:
          begin
            wregistro := wregistro + Space(3);
            wregistro := wregistro + FormatFloat('000000000000000', ValorIR * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorISS * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorIOF * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Deducoes * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Acrescimos * 100);
            wregistro := wregistro + FormatFloat('00000', ContaCorrente.Agencia.Codigo);
            wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Agencia.DV), 1);
            wregistro := wregistro + FormatFloat('000000000000', ContaCorrente.Conta.Numero);
            wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Conta.DV), 1);
            wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.DV), 1);
            wregistro := wregistro + FormatFloat('000000000000000', ValorINSS * 100);
            wregistro := wregistro + Space(113);
          end;
      else
        begin
          wregistro := wregistro + Space(3);
          wregistro := wregistro + FormatFloat('000000000000000', ValorIR * 100);
          wregistro := wregistro + FormatFloat('000000000000000', ValorISS * 100);
          wregistro := wregistro + FormatFloat('000000000000000', ValorIOF * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Deducoes * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Acrescimos * 100);
          wregistro := wregistro + FormatFloat('00000', ContaCorrente.Agencia.Codigo);
          wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Agencia.DV), 1);
          wregistro := wregistro + FormatFloat('000000', ContaCorrente.Conta.TipoConta);
          wregistro := wregistro + FormatFloat('000000', ContaCorrente.Conta.Numero);
          wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Conta.DV), 1);
          wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.DV), 1);
          wregistro := wregistro + FormatFloat('000000000000000', ValorINSS * 100);
          wregistro := wregistro + Space(113);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoD(mSegmentoDList: TSegmentoDList);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to {FPagFor.Lote.Items[I].SegmentoD}mSegmentoDList.Count - 1 do
  begin
    with {FPagFor.Lote.Items[I].SegmentoD}mSegmentoDList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'D';
      wregistro := wregistro + Space(3);

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            wregistro := wregistro + FormatFloat('000000', PeriodoCompetencia); // MMYYYY
            wregistro := wregistro + PadRight(CentroCusto, 15);
            wregistro := wregistro + PadRight(CodigoFuncionario, 15);
            wregistro := wregistro + PadRight(Cargo, 30);

            if FeriasInicio = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', FeriasInicio);

            if FeriasFim = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', FeriasFim);

            wregistro := wregistro + FormatFloat('00', DependentesIR);
            wregistro := wregistro + FormatFloat('00', DependentesSalarioFamilia);
            wregistro := wregistro + FormatFloat('00', HorasSemanais);
            wregistro := wregistro + FormatFloat('000000000000000', SalarioContribuicao * 100);
            wregistro := wregistro + FormatFloat('000000000000000', FGTS * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorCredito * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorDebito * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorLiquido * 100);
            wregistro := wregistro + FormatFloat('000000000000000', ValorBase * 100);
            wregistro := wregistro + FormatFloat('000000000000000', BaseCalculoIRRF * 100);
            wregistro := wregistro + FormatFloat('000000000000000', BaseCalculoFGTS * 100);
            wregistro := wregistro + PadRight(Disponibilizacao, 2);
            wregistro := wregistro + Space(3);
            wregistro := wregistro + Space(10);
          end;
      else
        begin
          // Falta implementar
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoE(mSegmentoEList: TSegmentoEList);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to {FPagFor.Lote.Items[I].SegmentoE}mSegmentoEList.Count - 1 do
  begin
    with {FPagFor.Lote.Items[I].SegmentoE}mSegmentoEList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'E';
      wregistro := wregistro + Space(3);
      wregistro := wregistro + TpMovimentoPagtoToStr(Movimento);
      wregistro := wregistro + PadRight(InformacaoComplementar, 200);
      wregistro := wregistro + Space(12);
      wregistro := wregistro + Space(10);

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoF(mSegmentoFList: TSegmentoFList);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to {FPagFor.Lote.Items[I].SegmentoF}mSegmentoFList.Count - 1 do
  begin
    with {FPagFor.Lote.Items[I].SegmentoF}mSegmentoFList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'F';
      wregistro := wregistro + Space(3);
      wregistro := wregistro + PadRight(InformacaoComplementar, 144);
      wregistro := wregistro + Space(69);
      wregistro := wregistro + Space(10);

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoJ(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoJ.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoJ.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'J';
      wregistro := wregistro + '0';
      wregistro := wregistro + InMovimentoToStr(CodMovimento);
      wregistro := wregistro + CodigoBarras;
      wregistro := wregistro + PadRight(TiraAcentos(NomeCedente), 30);

      if DataVencimento = 0 then
        wregistro := wregistro + '00000000'
      else
        wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);

      case FPagFor.Geral.Banco of
        pagBancoDoBrasil:
          begin
            wregistro := wregistro + FormatFloat('000000000000000', ValorTitulo * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Desconto * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Acrescimo * 100);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);
            wregistro := wregistro + FormatFloat('000000000000000', ValorPagamento * 100);
            wregistro := wregistro + '000000000000000';
            wregistro := wregistro + PadRight(TiraAcentos(ReferenciaSacado), 20);
            wregistro := wregistro + PadRight(NossoNumero, 20);
            wregistro := wregistro + FormatFloat('00', CodigoMoeda);
            wregistro := wregistro + Space(6);
            wregistro := wregistro + '0000000000';
         end;
        pagHSBC:
          begin
            wregistro := wregistro + '  ';
            wregistro := wregistro + FormatFloat('0000000000000', ValorTitulo * 100);
            wregistro := wregistro + '  ';
            wregistro := wregistro + FormatFloat('0000000000000', Desconto * 100);
            wregistro := wregistro + '  ';
            wregistro := wregistro + FormatFloat('0000000000000', Acrescimo * 100);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);
            wregistro := wregistro + '  ';
            wregistro := wregistro + FormatFloat('0000000000000', ValorPagamento * 100);
            wregistro := wregistro + '  ';
            wregistro := wregistro + FormatFloat('0000000000000', QtdeMoeda * 100000);
            wregistro := wregistro + PadRight(TiraAcentos(ReferenciaSacado), 20);
            wregistro := wregistro + Space(20);
            wregistro := wregistro + Space(2);
            wregistro := wregistro + 'N'; // Comprovante de Pagto. Emissão Individual “S=Sim ou N=Não”
            wregistro := wregistro + Space(15);
          end;

        pagItau, pagSantander:
          begin
            wregistro := wregistro + FormatFloat('000000000000000', ValorTitulo * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Desconto * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Acrescimo * 100);

            if DataPagamento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);

            wregistro := wregistro + FormatFloat('000000000000000', ValorPagamento * 100);
            wregistro := wregistro + FormatFloat('000000000000000', QtdeMoeda * 100000);
            wregistro := wregistro + PadRight(TiraAcentos(ReferenciaSacado), 20);
            wregistro := wregistro + Space(13);
            wregistro := wregistro + Space(15);
            wregistro := wregistro + Space(10);
          end;
        pagSicred, pagBancoCECRED, pagBancoSafra:
          begin
            wregistro := wregistro + FormatFloat('000000000000000', ValorTitulo * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Desconto * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Acrescimo * 100);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);
            wregistro := wregistro + FormatFloat('000000000000000', ValorPagamento * 100);
            wregistro := wregistro + FormatFloat('000000000000000', QtdeMoeda * 100000);
            wregistro := wregistro + PadRight(TiraAcentos(ReferenciaSacado), 20);
            wregistro := wregistro + PadRight(TiraAcentos(NossoNumero), 20);
            wregistro := wregistro + FormatFloat('00', CodigoMoeda);
            wregistro := wregistro + Space(6);
            wregistro := wregistro + Space(10);
          end;
        pagBradesco:
          begin
            wregistro := wregistro + FormatFloat('000000000000000', ValorTitulo * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Desconto * 100);
            wregistro := wregistro + FormatFloat('000000000000000', Acrescimo * 100);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);
            wregistro := wregistro + FormatFloat('000000000000000', ValorPagamento * 100);
            wregistro := wregistro + FormatFloat('000000000000000', QtdeMoeda * 100000);
            wregistro := wregistro + PadRight(TiraAcentos(ReferenciaSacado), 20);
            wregistro := wregistro + PadRight(TiraAcentos(NossoNumero), 20);
            wregistro := wregistro + FormatFloat('00', CodigoMoeda);
            wregistro := wregistro + Space(6);
            wregistro := wregistro + Space(10);
          end;
      else
        begin
          wregistro := wregistro + '  ';
          wregistro := wregistro + FormatFloat('0000000000000', ValorTitulo * 100);
          wregistro := wregistro + '  ';
          wregistro := wregistro + FormatFloat('0000000000000', Desconto * 100);
          wregistro := wregistro + '  ';
          wregistro := wregistro + FormatFloat('0000000000000', Acrescimo * 100);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);
          wregistro := wregistro + '  ';
          wregistro := wregistro + FormatFloat('0000000000000', ValorPagamento * 100);
          wregistro := wregistro + '  ';
          wregistro := wregistro + FormatFloat('0000000000000', QtdeMoeda * 100000);
          wregistro := wregistro + PadRight(TiraAcentos(ReferenciaSacado), 20);
          wregistro := wregistro + Space(20);
          wregistro := wregistro + FormatFloat('00', CodigoMoeda);
          wregistro := wregistro + Space(6);
          wregistro := wregistro + Space(10);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {opcionais segmento J}
      GeraSegmentoJ52(SegmentoJ52);
      GeraSegmentoB(SegmentoB);
      GeraSegmentoC(SegmentoC);
      GeraSegmentoZ(SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoJ52(mSegmentoJ52List: TSegmentoJ52List);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to {FPagFor.Lote.Items[I].SegmentoJ52}mSegmentoJ52List.Count - 1 do
  begin
    with {FPagFor.Lote.Items[I].SegmentoJ52}mSegmentoJ52List.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      if (FPagFor.Geral.Banco = PagBradesco) or 
        (FPagFor.Geral.Banco = PagBancoDoBrasil) then
        Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'J';

      case FPagFor.Geral.Banco of
        PagBancoDoBrasil:
          begin
            wregistro := wregistro + ' ';
            wregistro := wregistro + InMovimentoToStr(CodMovimento);
          end;
        PagSicred:
          begin
            wregistro := wregistro + ' ';
            wregistro := wregistro + '01'; //Conforme orientado pelo sicredi o J-52 sempre sera 01 o codigo do movimento na remessa
          end;
        pagBradesco:
          begin
            wregistro := wregistro + ' ';
            wregistro := wregistro + '00';
          end;
      else
        begin
          wregistro := wregistro + TpMovimentoToStr(TipoMovimento);
          wregistro := wregistro + InMovimentoToStr(CodMovimento);
        end;
      end;

      wregistro := wregistro + '52';
      wregistro := wregistro + TpInscricaoToStr(Pagador.Inscricao.Tipo);
      wregistro := wregistro + TBStrZero(Pagador.Inscricao.Numero, 15);
      wregistro := wregistro + PadRight(Pagador.Nome, 40);
      wregistro := wregistro + TpInscricaoToStr(Beneficiario.Inscricao.Tipo);
      wregistro := wregistro + TBStrZero(Beneficiario.Inscricao.Numero, 15);
      wregistro := wregistro + PadRight(Beneficiario.Nome, 40);
      wregistro := wregistro + TpInscricaoToStr(SacadorAvalista.Inscricao.Tipo);
      wregistro := wregistro + TBStrZero(SacadorAvalista.Inscricao.Numero, 15);
      wregistro := wregistro + PadRight(SacadorAvalista.Nome, 40);
      wregistro := wregistro + Space(53);

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoN1(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoN1.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoN1.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'N';
      wregistro := wregistro + '0';
      wregistro := wregistro + InMovimentoToStr(SegmentoN.CodMovimento);

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            {Inicio -  dados tributo}
            wregistro := wregistro + TpTributoToStr(ttGPS); // gps  '01'
            wregistro := wregistro + CodigoPagamentoGpsToStr(CodigoPagamento);
            wregistro := wregistro + FormatFloat('000000', MesAnoCompetencia);
            wregistro := wregistro + TBStrZero(idContribuinte, 14);
            wregistro := wregistro + FormatFloat('00000000000000', ValorTributo * 100);
            wregistro := wregistro + FormatFloat('00000000000000', ValorOutrasEntidades * 100);
            wregistro := wregistro + FormatFloat('00000000000000', AtualizacaoMonetaria * 100);
            wregistro := wregistro + FormatFloat('00000000000000', SegmentoN.ValorPagamento * 100);

            if SegmentoN.DataPagamento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);

            wregistro := wregistro + Space(8);
            wregistro := wregistro + Space(50);
            wregistro := wregistro + Space(30);
            {Fim  -  dados tributo}
            wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
            wregistro := wregistro + Space(15);
            wregistro := wregistro + Space(10);
          end
      else
        begin
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NossoNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NomeContribuinte), 30);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);
          wregistro := wregistro + FormatFloat('000000000000000', SegmentoN.ValorPagamento * 100);
          wregistro := wregistro + FormatFloat('000000', Receita);
          wregistro := wregistro + FormatFloat('00', TipoContribuinte);
          wregistro := wregistro + TBStrZero(idContribuinte, 14);
          wregistro := wregistro + '17';
          wregistro := wregistro + FormatFloat('000000', Competencia);
          wregistro := wregistro + FormatFloat('000000000000000', ValorTributo * 100);
          wregistro := wregistro + FormatFloat('000000000000000', ValorOutrasEntidades * 100);
          wregistro := wregistro + FormatFloat('000000000000000', AtualizacaoMonetaria * 100);
          wregistro := wregistro + Space(45);
          if FPagFor.Geral.Banco = pagBancoDoBrasil then
            wregistro := wregistro + '0000000000'
          else
            wregistro := wregistro + Space(10);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoN2(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoN2.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoN2.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'N';
      wregistro := wregistro + '0';
      wregistro := wregistro + InMovimentoToStr(SegmentoN.CodMovimento);

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            {Inicio -  Dados tributo}
            wregistro := wregistro + TpTributoToStr(ttDARFNormal); // '02'; // darf normal
            wregistro := wregistro + FormatFloat('0000', Receita);
            wregistro := wregistro + TpInscricaoToStr(TipoContribuinte);
            wregistro := wregistro + TBStrZero(idContribuinte, 14);

            if Periodo = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', Periodo);

            wregistro := wregistro + PadRight(Referencia, 17);
            wregistro := wregistro + FormatFloat('00000000000000', ValorPrincipal * 100);
            wregistro := wregistro + FormatFloat('00000000000000', Multa * 100);
            wregistro := wregistro + FormatFloat('00000000000000', Juros * 100);
            wregistro := wregistro + FormatFloat('00000000000000', SegmentoN.ValorPagamento * 100);;

            if DataVencimento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);

            if SegmentoN.DataPagamento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);

            wregistro := wregistro + Space(30);
            wregistro := wregistro + PadRight(SegmentoN.NomeContribuinte, 30);
            {Fim  -  Dados tributo}
            wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
            wregistro := wregistro + Space(15);
            wregistro := wregistro + Space(10);
          end
      else
        begin
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NossoNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NomeContribuinte), 30);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);
          wregistro := wregistro + FormatFloat('000000000000000', SegmentoN.ValorPagamento * 100);
          wregistro := wregistro + FormatFloat('000000', Receita);
          wregistro := wregistro + TBStrZero(TpInscricaoToStr(TipoContribuinte), 2);
          wregistro := wregistro + TBStrZero(idContribuinte, 14);
          wregistro := wregistro + '16';
          wregistro := wregistro + FormatDateTime('ddmmyyyy', Periodo);
          wregistro := wregistro + TBStrZero(Referencia, 17);
          wregistro := wregistro + FormatFloat('000000000000000', ValorPrincipal * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Multa * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Juros * 100);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
          wregistro := wregistro + Space(18);
          if FPagFor.Geral.Banco = pagBancoDoBrasil then
            wregistro := wregistro + '0000000000'
          else
            wregistro := wregistro + Space(10);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoN3(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoN3.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoN3.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'N';
      wregistro := wregistro + '0';
      wregistro := wregistro + InMovimentoToStr(SegmentoN.CodMovimento);

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            {Inicio -  dados tributo}
            wregistro := wregistro + TpTributoToStr(ttDARFSimples); //'03'; //darf simples
            wregistro := wregistro + FormatFloat('0000', Receita);
            wregistro := wregistro + TpInscricaoToStr(TipoContribuinte);
            wregistro := wregistro + TBStrZero(idContribuinte, 14);

            if Periodo = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', Periodo);

            wregistro := wregistro + FormatFloat('000000000', ReceitaBruta * 100);
            wregistro := wregistro + FormatFloat('0000', Percentual * 100);
            wregistro := wregistro + Space(4);
            wregistro := wregistro + FormatFloat('00000000000000', ValorPrincipal * 100);
            wregistro := wregistro + FormatFloat('00000000000000', Multa * 100);
            wregistro := wregistro + FormatFloat('00000000000000', Juros * 100);
            wregistro := wregistro + FormatFloat('00000000000000', SegmentoN.ValorPagamento * 100);

            if DataVencimento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);

            if DataVencimento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);

            wregistro := wregistro + Space(30);
            wregistro := wregistro + PadRight(SegmentoN.NomeContribuinte, 30);
            {Fim  -  dados tributo}
            wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
            wregistro := wregistro + Space(15);
            wregistro := wregistro + Space(10);
          end
      else
        begin
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NossoNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NomeContribuinte), 30);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);
          wregistro := wregistro + FormatFloat('000000000000000', SegmentoN.ValorPagamento * 100);
          wregistro := wregistro + FormatFloat('000000', Receita);
          wregistro := wregistro + TBStrZero(TpInscricaoToStr(TipoContribuinte), 2);
          wregistro := wregistro + TBStrZero(idContribuinte, 14);
          wregistro := wregistro + '18';
          wregistro := wregistro + FormatDateTime('ddmmyyyy', Periodo);
          wregistro := wregistro + FormatFloat('000000000000000', ReceitaBruta * 100);
          wregistro := wregistro + FormatFloat('0000000', Percentual * 100);
          wregistro := wregistro + FormatFloat('000000000000000', ValorPrincipal * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Multa * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Juros * 100);
          wregistro := wregistro + Space(21);
          if FPagFor.Geral.Banco = pagBancoDoBrasil then
            wregistro := wregistro + '0000000000'
          else
            wregistro := wregistro + Space(10);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoN4(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoN4.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoN4.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'N';
      wregistro := wregistro + '0';
      wregistro := wregistro + InMovimentoToStr(SegmentoN.CodMovimento);

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            {Inicio -  Dados tributo}
            wregistro := wregistro + TpTributoToStr(ttGareICMS); // '05'; // GARE-SP
            wregistro := wregistro + FormatFloat('0000', Receita);
            wregistro := wregistro + TpInscricaoToStr(TipoContribuinte);
            wregistro := wregistro + TBStrZero(idContribuinte, 14);
            wregistro := wregistro + TBStrZero(InscEst, 12);
            wregistro := wregistro + TBStrZero(NumEtiqueta, 13);
            wregistro := wregistro + FormatFloat('000000', Referencia);
            wregistro := wregistro + TBStrZero(NumParcela, 13);
            wregistro := wregistro + FormatFloat('00000000000000', ValorReceita * 100);
            wregistro := wregistro + FormatFloat('00000000000000', Juros * 100);
            wregistro := wregistro + FormatFloat('00000000000000', Multa * 100);
            wregistro := wregistro + FormatFloat('00000000000000', SegmentoN.ValorPagamento * 100);

            if DataVencimento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);

            if SegmentoN.DataPagamento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);

            wregistro := wregistro + Space(11);
            wregistro := wregistro + PadRight(SegmentoN.NomeContribuinte, 30);
            {Fim  -  Dados tributo}
            wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
            wregistro := wregistro + Space(15);
            wregistro := wregistro + Space(10);
          end
      else
        begin
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NossoNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NomeContribuinte), 30);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);
          wregistro := wregistro + FormatFloat('000000000000000', SegmentoN.ValorPagamento * 100);
          wregistro := wregistro + FormatFloat('000000', Receita);
          wregistro := wregistro + TBStrZero(TpInscricaoToStr(TipoContribuinte), 2);
          wregistro := wregistro + TBStrZero(idContribuinte, 14);
          wregistro := wregistro + TpIndTributoToStr(FPagFor.Geral.idTributo);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
          wregistro := wregistro + TBStrZero(InscEst, 12);
          wregistro := wregistro + TBStrZero(NumEtiqueta, 13);
          wregistro := wregistro + FormatFloat('000000', Referencia);
          wregistro := wregistro + TBStrZero(NumParcela, 13);
          wregistro := wregistro + FormatFloat('000000000000000', ValorReceita * 100);
          wregistro := wregistro + FormatFloat('00000000000000', Juros * 100);
          wregistro := wregistro + FormatFloat('00000000000000', Multa * 100);
          wregistro := wregistro + Space(1);
          if FPagFor.Geral.Banco = pagBancoDoBrasil then
            wregistro := wregistro + '0000000000'
          else
            wregistro := wregistro + Space(10);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoN567(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoN567.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoN567.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'N';
      wregistro := wregistro + '0';
      wregistro := wregistro + InMovimentoToStr(SegmentoN.CodMovimento);

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            {Inicio -  dados tributo}
            case Tributo of
              itIPVA:
                wregistro := wregistro +  TpTributoToStr(ttIPVA);

              itLicenciamento:
                wregistro := wregistro +  TpTributoToStr(ttLicenciamento);

              itDPVAT:
                wregistro := wregistro +  TpTributoToStr(ttDPVAT);
            else
              wregistro := wregistro +  '  '; // 7-IPVA / 8-DPVAT
            end;

            wregistro := wregistro + Space(4);
            wregistro := wregistro + TpInscricaoToStr(TipoContribuinte);
            wregistro := wregistro + TBStrZero(idContribuinte, 14);
            wregistro := wregistro + FormatFloat('0000', Exercicio);

            if Length(Renavam) > 9 then
              wregistro := wregistro + '000000000'
            else
              wregistro := wregistro + TBStrZero(Renavam, 9);

            wregistro := wregistro + PadRight(TiraAcentos(Estado), 2);

            if Tributo = itDPVAT then
              wregistro := wregistro + '00000'
            else
              wregistro := wregistro + FormatFloat('00000', Municipio);

            wregistro := wregistro + PadRight(TiraAcentos(Placa), 7);
            wregistro := wregistro + PadRight(OpcaoPagamento, 1);
            wregistro := wregistro + FormatFloat('00000000000000', ValorTributo * 100);
            wregistro := wregistro + FormatFloat('00000000000000', Desconto * 100);
            wregistro := wregistro + FormatFloat('00000000000000', SegmentoN.ValorPagamento * 100);

            if (Tributo = itDPVAT) or (DataVencimento = 0) then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);

            if SegmentoN.DataPagamento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);

            wregistro := wregistro + Space(29);
            wregistro := wregistro + TBStrZero(Renavam, 12);
            wregistro := wregistro + PadRight(SegmentoN.NomeContribuinte, 30);
            {Fim  -  Dados tributo}
            wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
            wregistro := wregistro + Space(15);
            wregistro := wregistro + Space(10);
          end
      else
        begin
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NossoNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NomeContribuinte), 30);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);
          wregistro := wregistro + FormatFloat('000000000000000', SegmentoN.ValorPagamento * 100);
          wregistro := wregistro + FormatFloat('000000', Receita);
          wregistro := wregistro + TBStrZero(TpInscricaoToStr(TipoContribuinte), 2);
          wregistro := wregistro + TBStrZero(idContribuinte, 14);
          wregistro := wregistro + TpIndTributoToStr(FPagFor.Geral.idTributo);
          wregistro := wregistro + FormatFloat('0000', Exercicio);
          wregistro := wregistro + TBStrZero(Renavam, 9);
          wregistro := wregistro + PadRight(TiraAcentos(Estado), 2);
          wregistro := wregistro + FormatFloat('00000', Municipio);
          wregistro := wregistro + PadRight(TiraAcentos(Placa), 7);

          case FPagFor.Geral.idTributo of
            itIPVA:
              begin // IPVA
                wregistro := wregistro + PadRight(TiraAcentos(OpcaoPagamento), 1);
                wregistro := wregistro + Space(68);
              end;

            itLicenciamento:
              begin // Licenciamento
                wregistro := wregistro + '5';
                wregistro := wregistro + PadRight(TiraAcentos(OpcaoRetirada), 1);
                wregistro := wregistro + Space(67);
              end;

            itDPVAT:
              begin // DPVAT
                wregistro := wregistro + '5';
                wregistro := wregistro + Space(68);
              end;
          else
            begin
              wregistro := wregistro + ' ';
              wregistro := wregistro + Space(68);
            end;
          end;

          if FPagFor.Geral.Banco = pagBancoDoBrasil then
            wregistro := wregistro + '0000000000'
          else
            wregistro := wregistro + Space(10);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoN8(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoN8.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoN8.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'N';
      wregistro := wregistro + '0';
      wregistro := wregistro + InMovimentoToStr(SegmentoN.CodMovimento);

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            {Inicio -  Dados tributo}
            wregistro := wregistro + '04'; // DARJ
            wregistro := wregistro + FormatFloat('0000', Receita);
            wregistro := wregistro + TpInscricaoToStr(TipoContribuinte);
            wregistro := wregistro + TBStrZero(idContribuinte, 14);
            wregistro := wregistro + TBStrZero(InscEst, 8);
            wregistro := wregistro + TBStrZero(Origem, 16);
            wregistro := wregistro + Space(1);
            wregistro := wregistro + FormatFloat('00000000000000', ValorPrincipal * 100);
            wregistro := wregistro + FormatFloat('00000000000000', AtualizacaoMonetaria * 100);
            wregistro := wregistro + FormatFloat('00000000000000', Mora * 100);
            wregistro := wregistro + FormatFloat('00000000000000', Multa * 100);
            wregistro := wregistro + FormatFloat('00000000000000', SegmentoN.ValorPagamento * 100);

            if DataVencimento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);

            if SegmentoN.DataPagamento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);

            wregistro := wregistro + FormatFloat('000000', PeriodoParcela);
            wregistro := wregistro + Space(10);
            wregistro := wregistro + PadRight(SegmentoN.NomeContribuinte, 30);
            {Fim  -  Dados tributo}
            wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
            wregistro := wregistro + Space(15);
            wregistro := wregistro + Space(10);
          end
      else
        begin
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NossoNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.NomeContribuinte), 30);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);
          wregistro := wregistro + FormatFloat('000000000000000', SegmentoN.ValorPagamento * 100);
          wregistro := wregistro + FormatFloat('000000', Receita);
          wregistro := wregistro + TBStrZero(TpInscricaoToStr(TipoContribuinte), 2);
          wregistro := wregistro + TBStrZero(idContribuinte, 14);
          wregistro := wregistro + TBStrZero(InscEst, 8);
          wregistro := wregistro + TBStrZero(Origem, 16);
          wregistro := wregistro + FormatFloat('000000000000000', ValorPrincipal * 100);
          wregistro := wregistro + FormatFloat('000000000000000', AtualizacaoMonetaria * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Mora * 100);
          wregistro := wregistro + FormatFloat('000000000000000', Multa * 100);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
          wregistro := wregistro + FormatFloat('000000', PeriodoParcela);
          if FPagFor.Geral.Banco = pagBancoDoBrasil then
            wregistro := wregistro + '0000000000'
          else
            wregistro := wregistro + Space(10);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoN9(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoN9.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoN9.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'N';
      wregistro := wregistro + '0';
      wregistro := wregistro + InMovimentoToStr(SegmentoN.CodMovimento);

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            {Inicio -  Dados tributo}
            wregistro := wregistro + '11'; // FGTS- GRF/GRRF/GRDE
            wregistro := wregistro + FormatFloat('0000', Receita);
            wregistro := wregistro + TpInscricaoToStr(TipoContribuinte);
            wregistro := wregistro + TBStrZero(idContribuinte, 14);
            wregistro := wregistro + PadRight(CodigoBarras, 48);
            wregistro := wregistro + FormatFloat('0000000000000000', Identificador);
            wregistro := wregistro + FormatFloat('000000000', Lacre);
            wregistro := wregistro + FormatFloat('00', LacreDigito);
            wregistro := wregistro + PadRight(SegmentoN.NomeContribuinte, 30);
            if SegmentoN.DataPagamento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', SegmentoN.DataPagamento);
            wregistro := wregistro + FormatFloat('00000000000000', SegmentoN.ValorPagamento * 100);
            wregistro := wregistro + Space(30);
            {Fim  -  Dados tributo}
            wregistro := wregistro + PadRight(TiraAcentos(SegmentoN.SeuNumero), 20);
            wregistro := wregistro + Space(15);
            wregistro := wregistro + Space(10);
          end
      else
        begin
          // falta implementar
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {Adicionais segmento N}
      GeraSegmentoB(SegmentoN.SegmentoB);
      GeraSegmentoW(SegmentoN.SegmentoW);
      GeraSegmentoZ(SegmentoN.SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoO(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoO.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoO.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);
      Inc(FSequencialDeLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'O';
      wregistro := wregistro + '0';
      wregistro := wregistro + InMovimentoToStr(CodMovimento);

      case FPagFor.Geral.Banco of
        pagBancoDoBrasil:
          begin
            wregistro := wregistro + PadRight(CodigoBarras, 44);
            wregistro := wregistro + PadRight(TiraAcentos(NomeConcessionaria), 30);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);
            wregistro := wregistro + FormatFloat('000000000000000', ValorPagamento * 100);
            wregistro := wregistro + PadRight(TiraAcentos(SeuNumero), 20);
            wregistro := wregistro + PadRight(TiraAcentos(NossoNumero), 20);
            wregistro := wregistro + Space(68);
            wregistro := wregistro + '0000000000';
          end;

        pagSantander, pagSicred, pagBancoSafra:
          begin
            wregistro := wregistro + PadRight(CodigoBarras, 44);
            wregistro := wregistro + PadRight(TiraAcentos(NomeConcessionaria), 30);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);
            wregistro := wregistro + FormatFloat('000000000000000', ValorPagamento * 100);
            wregistro := wregistro + PadRight(TiraAcentos(SeuNumero), 20);
            wregistro := wregistro + PadRight(TiraAcentos(NossoNumero), 20);
            wregistro := wregistro + Space(68);
            wregistro := wregistro + Space(10);
          end;

        pagHSBC:
          begin
            wregistro := wregistro + PadRight(CodigoBarras, 44);
            wregistro := wregistro + PadRight(TiraAcentos(NomeConcessionaria), 30);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
            wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);
            wregistro := wregistro + FormatFloat('000000000000000', ValorPagamento * 100);
            wregistro := wregistro + PadRight(TiraAcentos(SeuNumero), 20);
            wregistro := wregistro + PadRight(TiraAcentos(NossoNumero), 19);
            wregistro := wregistro + 'N'; // Comprovante Pagamento Emissão Individual  “S = Sim” ou “N = Não”
            wregistro := wregistro + Space(78);
          end;

        pagItau:
          begin
            wregistro := wregistro + PadRight(CodigoBarras, 48);
            wregistro := wregistro + PadRight(TiraAcentos(NomeConcessionaria), 30);

            if DataVencimento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);

            wregistro := wregistro + 'REA';
            wregistro := wregistro + '000000000000000';
            wregistro := wregistro + FormatFloat('000000000000000', ValorPagamento * 100);

            if DataPagamento = 0 then
              wregistro := wregistro + '00000000'
            else
              wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);

            wregistro := wregistro + '000000000000000';
            wregistro := wregistro + Space(3);
            wregistro := wregistro + '000000000';
            wregistro := wregistro + Space(3);
            wregistro := wregistro + PadRight(TiraAcentos(SeuNumero), 20);
            wregistro := wregistro + Space(21);
            wregistro := wregistro + '000000000000000';
            wregistro := wregistro + '0000000000';
          end;
      else
        begin
          wregistro := wregistro + PadRight(CodigoBarras, 44);
          wregistro := wregistro + PadRight(TiraAcentos(NomeConcessionaria), 30);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', DataVencimento);
          wregistro := wregistro + FormatDateTime('ddmmyyyy', DataPagamento);
          wregistro := wregistro + FormatFloat('000000000000000', ValorPagamento * 100);
          wregistro := wregistro + PadRight(TiraAcentos(SeuNumero), 20);
          wregistro := wregistro + PadRight(TiraAcentos(NossoNumero), 19);
          wregistro := wregistro + 'N'; // Comprovante Pagamento Emissão Individual  “S = Sim” ou “N = Não”
          wregistro := wregistro + Space(78);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;

      {opcionais segmento O}
      GeraSegmentoZ(SegmentoZ);
    end;
  end;
end;

procedure TPagForW.GeraSegmentoP(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoP.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoP.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FQtdeRegistrosLote);
      wregistro := wregistro + 'P';
      wregistro := wregistro + Space(1);
      wregistro := wregistro + InMovimentoToStr(CodMovimento);

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            wregistro := wregistro + FormatFloat('00000', ContaCorrente.Agencia.Codigo);
            wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Agencia.DV), 1);
            wregistro := wregistro + FormatFloat('000000', ContaCorrente.Conta.TipoConta);
            wregistro := wregistro + FormatFloat('000000', ContaCorrente.Conta.Numero);
            wregistro := wregistro + ' ';

            if PadRight(TiraAcentos(ContaCorrente.Conta.DV), 1) <> ' ' then
              wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Conta.DV), 1)
            else
              wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.DV), 1);
          end;
        pagBancoDoBrasil:
          begin
            wregistro := wregistro + FormatFloat('00000', ContaCorrente.Agencia.Codigo);
            wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Agencia.DV), 1);
            wregistro := wregistro + FormatFloat('000000000000', ContaCorrente.Conta.Numero);
            wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Conta.DV), 1);
            wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.DV), 1);
          end;
      else
        begin
          wregistro := wregistro + FormatFloat('00000', ContaCorrente.Agencia.Codigo);
          wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Agencia.DV), 1);
          wregistro := wregistro + FormatFloat('000000', ContaCorrente.Conta.TipoConta);
          wregistro := wregistro + FormatFloat('000000', ContaCorrente.Conta.Numero);
          wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.Conta.DV), 1);
          wregistro := wregistro + PadRight(TiraAcentos(ContaCorrente.DV), 1);
        end;
      end;

      wregistro := wregistro + PadRight(TiraAcentos(NossoNumero), 20);
      wregistro := wregistro + FormatFloat('0', Cobranca.Carteira);
      wregistro := wregistro + FormatFloat('0', Cobranca.Cadastramento);
      wregistro := wregistro + PadRight(Cobranca.TipoDocumento, 1);
      wregistro := wregistro + FormatFloat('0', Cobranca.EmissaoBoleto);
      wregistro := wregistro + PadRight(Cobranca.DistribuicaoBoleto, 1);
      wregistro := wregistro + PadRight(TiraAcentos(NumeroDocumento), 15);

      if Vencimento = 0 then
        wregistro := wregistro + '00000000'
      else
        wregistro := wregistro + FormatDateTime('ddmmyyyy', Vencimento);

      wregistro := wregistro + FormatFloat('000000000000000', ValorTitulo * 100);
      wregistro := wregistro + FormatFloat('00000', AgenciaCobradora);
      wregistro := wregistro + PadRight(DV, 1);
      wregistro := wregistro + FormatFloat('00', EspecieTitulo);
      wregistro := wregistro + PadRight(Aceito, 1);

      if DataEmissao = 0 then
        wregistro := wregistro + '00000000'
      else
        wregistro := wregistro + FormatDateTime('ddmmyyyy', DataEmissao);

      wregistro := wregistro + FormatFloat('0', Juros.Codigo);

      if Juros.Data = 0 then
        wregistro := wregistro + '00000000'
      else
        wregistro := wregistro + FormatDateTime('ddmmyyyy', Juros.Data);

      wregistro := wregistro + FormatFloat('000000000000000', Juros.Valor * 100);
      wregistro := wregistro + FormatFloat('0', Desconto.Codigo);

      if Desconto.Data = 0 then
        wregistro := wregistro + '00000000'
      else
        wregistro := wregistro + FormatDateTime('ddmmyyyy', Desconto.Data);

      wregistro := wregistro + FormatFloat('000000000000000', Desconto.Valor * 100);
      wregistro := wregistro + FormatFloat('000000000000000', ValorIOF * 100);
      wregistro := wregistro + FormatFloat('000000000000000', ValorAbatimento * 100);
      wregistro := wregistro + PadRight(TiraAcentos(UsoEmpresaCedente), 25);
      wregistro := wregistro + FormatFloat('0', CodigoProtesto);
      wregistro := wregistro + FormatFloat('00', PrazoProtesto);
      wregistro := wregistro + FormatFloat('0', CodigoBaixaDevolucao);
      wregistro := wregistro + FormatFloat('000', PrazoBaixaDevolucao);
      wregistro := wregistro + FormatFloat('00', CodigoMoeda);
      wregistro := wregistro + FormatFloat('0000000000', NumeroContrato);
      wregistro := wregistro + Space(1);

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoQ(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoQ.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoQ.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FQtdeRegistrosLote);
      wregistro := wregistro + 'Q';
      wregistro := wregistro + ' ';
      wregistro := wregistro + InMovimentoToStr(CodMovimento);
      wregistro := wregistro + TpInscricaoToStr(Sacado.Inscricao.Tipo);
      wregistro := wregistro + TBStrZero(Sacado.Inscricao.Numero, 15);
      wregistro := wregistro + PadRight(TiraAcentos(Sacado.Nome), 40);
      wregistro := wregistro + PadRight(TiraAcentos(Sacado.Endereco), 40);
      wregistro := wregistro + PadRight(TiraAcentos(Sacado.Bairro), 15);
      wregistro := wregistro + FormatFloat('00000000', Sacado.CEP);
      wregistro := wregistro + PadRight(TiraAcentos(Sacado.Cidade), 15);
      wregistro := wregistro + PadRight(Sacado.UF, 2);
      wregistro := wregistro + TpInscricaoToStr(Avalista.Inscricao.Tipo);
      wregistro := wregistro + TBStrZero(Avalista.Inscricao.Numero, 15);
      wregistro := wregistro + PadRight(TiraAcentos(Avalista.Nome), 40);
      wregistro := wregistro + BancoToStr(BancoCorrespondente);
      wregistro := wregistro + PadRight(TiraAcentos(NossoNumeroCorrespondente), 20);
      wregistro := wregistro + Space(8);

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoR(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoR.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoR.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FQtdeRegistrosLote);
      wregistro := wregistro + 'R';
      wregistro := wregistro + ' ';
      wregistro := wregistro + InMovimentoToStr(CodMovimento);
      wregistro := wregistro + FormatFloat('0', Desconto2.Codigo);

      if Desconto2.Data = 0 then
        wregistro := wregistro + '00000000'
      else
        wregistro := wregistro + FormatDateTime('ddmmyyyy', Desconto2.Data);

      wregistro := wregistro + FormatFloat('000000000000000', Desconto2.Valor * 100);
      wregistro := wregistro + FormatFloat('0', Desconto3.Codigo);

      if Desconto3.Data = 0 then
        wregistro := wregistro + '00000000'
      else
        wregistro := wregistro + FormatDateTime('ddmmyyyy', Desconto3.Data);

      wregistro := wregistro + FormatFloat('000000000000000', Desconto3.Valor * 100);
      wregistro := wregistro + FormatFloat('0', Multa.Codigo);

      if Multa.Data = 0 then
        wregistro := wregistro + '00000000'
      else
        wregistro := wregistro + FormatDateTime('ddmmyyyy', Multa.Data);

      wregistro := wregistro + FormatFloat('000000000000000', Multa.Valor * 100);
      wregistro := wregistro + PadRight(TiraAcentos(InformacaoaoSacado), 10);
      wregistro := wregistro + PadRight(TiraAcentos(Informacao3), 40);
      wregistro := wregistro + PadRight(TiraAcentos(Informacao4), 40);
      wregistro := wregistro + Space(20);
      wregistro := wregistro + PadRight(CodOcorrSacado, 8);
      wregistro := wregistro + BancoToStr(DebitoAutomatico.Banco);

      case DebitoAutomatico.Banco of
        pagItau:
          begin
            wregistro := wregistro + FormatFloat('00000', DebitoAutomatico.ContaCorrente.Agencia.Codigo);
            wregistro := wregistro + PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.Agencia.DV), 1);
            wregistro := wregistro + FormatFloat('000000', DebitoAutomatico.ContaCorrente.Conta.TipoConta);
            wregistro := wregistro + FormatFloat('000000', DebitoAutomatico.ContaCorrente.Conta.Numero);
            wregistro := wregistro + ' ';

            if PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.Conta.DV), 1) <> ' ' then
              wregistro := wregistro + PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.Conta.DV), 1)
            else
              wregistro := wregistro + PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.DV), 1);
          end;
        pagBancoDoBrasil:
          begin
            wregistro := wregistro + FormatFloat('00000', DebitoAutomatico.ContaCorrente.Agencia.Codigo);
            wregistro := wregistro + PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.Agencia.DV), 1);
            wregistro := wregistro + FormatFloat('000000000000', DebitoAutomatico.ContaCorrente.Conta.Numero);
            wregistro := wregistro + PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.Conta.DV), 1);
            wregistro := wregistro + PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.DV), 1);
          end;
      else
        begin
          wregistro := wregistro + FormatFloat('00000', DebitoAutomatico.ContaCorrente.Agencia.Codigo);
          wregistro := wregistro + PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.Agencia.DV), 1);
          wregistro := wregistro + FormatFloat('000000', DebitoAutomatico.ContaCorrente.Conta.TipoConta);
          wregistro := wregistro + FormatFloat('000000', DebitoAutomatico.ContaCorrente.Conta.Numero);
          wregistro := wregistro + PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.Conta.DV), 1);
          wregistro := wregistro + PadRight(TiraAcentos(DebitoAutomatico.ContaCorrente.DV), 1);
        end;
      end;

      wregistro := wregistro + FormatFloat('0', AvisoDebitoAutomatico);
      wregistro := wregistro + Space(9);

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoS(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoS.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoS.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FQtdeRegistrosLote);
      wregistro := wregistro + 'S';
      wregistro := wregistro + ' ';
      wregistro := wregistro + InMovimentoToStr(CodMovimento);
      wregistro := wregistro + FormatFloat('0', TipoImpressao);

      if TipoImpressao < 3 then
      begin
        wregistro := wregistro + FormatFloat('00', NumerodaLinha);
        wregistro := wregistro + PadRight(TiraAcentos(Mensagem), 140);
        wregistro := wregistro + FormatFloat('00', TipodeFonte);
        wregistro := wregistro + Space(78);
      end
      else
      begin
        wregistro := wregistro + PadRight(TiraAcentos(Informacao5), 40);
        wregistro := wregistro + PadRight(TiraAcentos(Informacao6), 40);
        wregistro := wregistro + PadRight(TiraAcentos(Informacao7), 40);
        wregistro := wregistro + PadRight(TiraAcentos(Informacao8), 40);
        wregistro := wregistro + PadRight(TiraAcentos(Informacao9), 40);
        wregistro := wregistro + Space(22);
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoW(mSegmentoWList: TSegmentoWList);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to {FPagFor.Lote.Items[I].SegmentoW}mSegmentoWList.Count - 1 do
  begin
    with {FPagFor.Lote.Items[I].SegmentoW}mSegmentoWList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'W';

      case FPagFor.Geral.Banco of
        pagItau:
          begin
            wregistro := wregistro + Space(2);
            wregistro := wregistro + PadRight(TiraAcentos(Informacoes1), 40);
            wregistro := wregistro + PadRight(TiraAcentos(Informacoes2), 40);
            wregistro := wregistro + PadRight(TiraAcentos(Informacoes3), 40);
            wregistro := wregistro + PadRight(TiraAcentos(Informacoes4), 40);
            wregistro := wregistro + Space(64);
          end
      else
        begin
          wregistro := wregistro + FormatFloat('0', ComplementoRegistro);
          wregistro := wregistro + PadRight(Informacoes1ou2, 1);
          wregistro := wregistro + PadRight(TiraAcentos(Informacoes1), 80);
          wregistro := wregistro + PadRight(TiraAcentos(Informacoes2), 80);

          if PagFGTS then // Se True Pagamento de FGTS
          begin
            wregistro := wregistro + '01';
            wregistro := wregistro + PadRight(TiraAcentos(CodReceita), 6);
            wregistro := wregistro + PadRight(TipoIdContribuinte, 2);
            wregistro := wregistro + TBStrZero(idContribuinte, 14);
            wregistro := wregistro + PadRight(TiraAcentos(Identificador), 16);
            wregistro := wregistro + PadRight(TiraAcentos(LacreConecSocial), 9);
            wregistro := wregistro + PadRight(TiraAcentos(LacreDV), 2);
            wregistro := wregistro + ' ';
          end
          else
            wregistro := wregistro + Space(50);

          wregistro := wregistro + Space(2);
          wregistro := wregistro + Space(10);
        end;
      end;

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoY(I: Integer);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to FPagFor.Lote.Items[I].SegmentoY.Count - 1 do
  begin
    with FPagFor.Lote.Items[I].SegmentoY.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FQtdeRegistrosLote);
      wregistro := wregistro + 'Y';
      wregistro := wregistro + ' ';
      wregistro := wregistro + InMovimentoToStr(CodMovimento);
      wregistro := wregistro + FormatFloat('00', CodRegistro);

      { Incompleto }

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.GeraSegmentoZ(mSegmentoZList: TSegmentoZList);
var
  wregistro: string;
  J: Integer;
begin
  for J := 0 to {FPagFor.Lote.Items[I].SegmentoZ}mSegmentoZList.Count - 1 do
  begin
    with {FPagFor.Lote.Items[I].SegmentoZ}mSegmentoZList.Items[J] do
    begin
      Inc(FQtdeRegistros);
      Inc(FQtdeRegistrosLote);

      wregistro := BancoToStr(FPagFor.Geral.Banco);
      wregistro := wregistro + FormatFloat('0000', FQtdeLotes);
      wregistro := wregistro + '3';
      wregistro := wregistro + FormatFloat('00000', FSequencialDeLote);
      wregistro := wregistro + 'Z';
      wregistro := wregistro + PadRight(TiraAcentos(Autenticacao), 64);
      wregistro := wregistro + PadRight(TiraAcentos(SeuNumero), 25);
      wregistro := wregistro + Space(137);

      WriteRecord(wregistro);
      FArquivoTXT := FArquivoTXT + sLineBreak + wregistro;
    end;
  end;
end;

procedure TPagForW.LimparRegistros;
begin
//
end;

function TPagForW.ObterNomeArquivo: string;
begin
//
end;

procedure TPagForW.WriteRecord(const Rec: String);
begin
  if Length(Rec) <> 240 then
    raise Exception.Create('Registro inválido!' + #13 + 'Deve conter 240 posições.' + #13 + 'Registro: [' + Rec + ']' + #13 + 'possui ' + IntToStr
        (Length(Rec)) + ' posições.');
end;

end.
