{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibSATRespostas;

interface

uses
  SysUtils, Classes,
  ACBrSat, ACBrLibResposta;

type

  { TPadraoSATResposta }
  TPadraoSATResposta = class(TACBrLibResposta<TACBrSAT>)
  private
    FArquivo: String;
    FXML: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property Arquivo: String read FArquivo write FArquivo;
    property XML: String read FXML write FXML;

  end;

  { TACBrLibSATResposta }
  TACBrLibSATResposta = class(TACBrLibRespostaBase)
  private
    FCodigoDeErro: Integer;
    FNumeroSessao : Integer ;
    FCodigoDeRetorno : Integer ;
    FMensagemRetorno : String;
    FCodigoSEFAZ : Integer ;
    FMensagemSEFAZ : String;
    FRetorno : String ;

  public
    constructor Create( const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const SAT: TACBrSAT);

  published
    property NumeroSessao : Integer read FNumeroSessao write FNumeroSessao;
    property CodigoDeRetorno : Integer read  FCodigoDeRetorno write FCodigoDeRetorno;
    property CodigoDeErro : Integer read  FCodigoDeErro write FCodigoDeErro;
    property MensagemRetorno : String read FMensagemRetorno write FMensagemRetorno;
    property CodigoSEFAZ : Integer read  FCodigoSEFAZ write FCodigoSEFAZ;
    property MensagemSEFAZ : String read FMensagemSEFAZ write FMensagemSEFAZ;
    property Retorno : String read FRetorno write FRetorno;

  end;

  { TRetornoConsultarSessao }
  TRetornoConsultarSessao = class(TPadraoSATResposta)
  private
    FnCFe: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const SAT: TACBrSAT); override;

  published
    property nCFe: String read FnCFe write FnCFe;

  end;

  { TRetornoConsultarSessaoCancelado }
  TRetornoConsultarSessaoCancelado = class(TPadraoSATResposta)
  private
    FnCFeCanc: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const SAT: TACBrSAT); override;

  published
    property nCFeCanc: String read FnCFeCanc write FnCFeCanc;

  end;

  { TRetornoCriarCFe }
  TRetornoCriarCFe = class(TPadraoSATResposta)
  private
    FnCFe: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const SAT: TACBrSAT); override;

  published
    property nCFe: String read FnCFe write FnCFe;

  end;

  { TRetornoCancelarCFe }
  TRetornoCancelarCFe = class(TPadraoSATResposta)
  private
    FResultado: String;
    FNumeroSessao: Integer;
    FCodigoDeRetorno: Integer;
    FRetornoStr: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const SAT: TACBrSAT); override;

  published
    property Resultado: String read FResultado write FResultado;
    property NumeroSessao: Integer read FnumeroSessao write FnumeroSessao;
    property CodigoDeRetorno: Integer read FCodigoDeRetorno write FCodigoDeRetorno;
    property RetornoStr: String read FRetornoStr write FRetornoStr;

  end;

  { TRetornoTesteFimaFim }
  TRetornoTesteFimaFim = class(TPadraoSATResposta)
  private
    FResultado: String;
    FNumeroSessao: Integer;
    FCodigoDeRetorno: Integer;
    FRetornoStr: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const SAT: TACBrSAT); override;

  published
    property Resultado: String read FResultado write FResultado;
    property NumeroSessao: Integer read FnumeroSessao write FnumeroSessao;
    property CodigoDeRetorno: Integer read FCodigoDeRetorno write FCodigoDeRetorno;
    property RetornoStr: String read FRetornoStr write FRetornoStr;

  end;

  { TRetornoEnvio }
  TRetornoEnvio = class(TPadraoSATResposta)
  private
    FResultado: String;
    FNumeroSessao: Integer;
    FCodigoDeRetorno: Integer;
    FRetornoStr: String;
    FnCFe: Integer;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const SAT: TACBrSAT); override;

  published
    property Resultado: String read FResultado write FResultado;
    property NumeroSessao: Integer read FnumeroSessao write FnumeroSessao;
    property CodigoDeRetorno: Integer read FCodigoDeRetorno write FCodigoDeRetorno;
    property RetornoStr: String read FRetornoStr write FRetornoStr;
    property nCFe : integer read FnCFe write FnCFe;
  end;

  { TRetornoStatusSAT }
  TRetornoStatusSAT = class(TACBrLibRespostaBase)
  private
    FNSERIE          : String;
    FLAN_MAC         : String;
    FSTATUS_LAN      : String;
    FNIVEL_BATERIA   : String;
    FMT_TOTAL        : String;
    FMT_USADA        : String;
    FDH_ATUAL        : TDateTime;
    FVER_SB          : String;
    FVER_LAYOUT      : String;
    FULTIMO_CFe      : String;
    FLISTA_INICIAL   : String;
    FLISTA_FINAL     : String;
    FDH_CFe          : TDateTime;
    FDH_ULTIMA       : TDateTime;
    FCERT_EMISSAO    : TDateTime;
    FCERT_VENCIMENTO : TDateTime;
    FESTADO_OPERACAO : String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const SAT: TACBrSAT);

  published
    property NSERIE           : String    read  FNSERIE           write FNSERIE;
    property LAN_MAC          : String    read  FLAN_MAC          write FLAN_MAC;
    property STATUS_LAN       : String    read  FSTATUS_LAN       write FSTATUS_LAN;
    property NIVEL_BATERIA    : String    read  FNIVEL_BATERIA    write FNIVEL_BATERIA;
    property MT_TOTAL         : String    read  FMT_TOTAL         write FMT_TOTAL;
    property MT_USADA         : String    read  FMT_USADA         write FMT_USADA;
    property DH_ATUAL         : TDateTime read  FDH_ATUAL         write FDH_ATUAL;
    property VER_SB           : String    read  FVER_SB           write FVER_SB;
    property VER_LAYOUT       : String    read  FVER_LAYOUT       write FVER_LAYOUT;
    property ULTIMO_CFe       : String    read  FULTIMO_CFe       write FULTIMO_CFe;
    property LISTA_INICIAL    : String    read  FLISTA_INICIAL    write FLISTA_INICIAL;
    property LISTA_FINAL      : String    read  FLISTA_FINAL      write FLISTA_FINAL;
    property DH_CFe           : TDateTime read  FDH_CFe           write FDH_CFe;
    property DH_ULTIMA        : TDateTime read  FDH_ULTIMA        write FDH_ULTIMA;
    property CERT_EMISSAO     : TDateTime read  FCERT_EMISSAO     write FCERT_EMISSAO;
    property CERT_VENCIMENTO  : TDateTime read  FCERT_VENCIMENTO  write FCERT_VENCIMENTO;
    property ESTADO_OPERACAO  : String    read  FESTADO_OPERACAO  write FESTADO_OPERACAO;


  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrLibSATConsts;

{ TRetornoStatusSAT }
constructor TRetornoStatusSAT.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoStatusSAT, ATipo, AFormato);
end;

procedure TRetornoStatusSAT.Processar(const SAT: TACBrSAT);
begin
  with SAT.Status do
  begin
    Self.NSERIE := NSERIE;
    Self.LAN_MAC := LAN_MAC;
    Self.STATUS_LAN := StatusLanToStr(STATUS_LAN);
    Self.NIVEL_BATERIA := NivelBateriaToStr(NIVEL_BATERIA);
    Self.MT_TOTAL := MT_TOTAL;
    Self.MT_USADA := MT_USADA;
    Self.DH_ATUAL := DH_ATUAL;
    Self.VER_SB := VER_SB;
    Self.VER_LAYOUT := VER_LAYOUT;
    Self.ULTIMO_CFe := ULTIMO_CFe;
    Self.LISTA_INICIAL := LISTA_INICIAL;
    Self.LISTA_FINAL := LISTA_FINAL;
    Self.DH_CFe := DH_CFe;
    Self.DH_ULTIMA := DH_ULTIMA;
    Self.CERT_EMISSAO := CERT_EMISSAO;
    Self.CERT_VENCIMENTO := CERT_VENCIMENTO;
    Self.ESTADO_OPERACAO := EstadoOperacaoToStr(ESTADO_OPERACAO);
  end;
end;

{ TRetornoEnvio }
constructor TRetornoEnvio.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoENVIO, ATipo, AFormato);
end;

procedure TRetornoEnvio.Processar(const SAT: TACBrSAT);
begin
  Self.NumeroSessao     := SAT.Resposta.numeroSessao;
  Self.CodigoDeRetorno  := SAT.Resposta.codigoDeRetorno;
  Self.RetornoStr       := SAT.Resposta.RetornoStr;
  Self.XML              := SAT.CFe.AsXMLString;
  Self.nCFe             := SAT.CFe.ide.nCFe;

  if (SAT.CFe.NomeArquivo <> '') and FileExists(SAT.CFe.NomeArquivo) then
    Self.Arquivo:= SAT.CFe.NomeArquivo;
end;

{ TRetornoTesteFimaFim }
constructor TRetornoTesteFimaFim.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoTESTEFIMAFIM, ATipo, AFormato);
end;

procedure TRetornoTesteFimaFim.Processar(const SAT: TACBrSAT);
begin
  with SAT do
  begin
    Self.NumeroSessao := Resposta.numeroSessao;
    Self.CodigoDeRetorno := Resposta.codigoDeRetorno;
    Self.RetornoStr := Resposta.RetornoStr;
    Self.XML := CFe.AsXMLString;
  end;
end;

{ TRetornoCancelarCFe }
constructor TRetornoCancelarCFe.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoCFeCancelado, ATipo, AFormato);
end;

procedure TRetornoCancelarCFe.Processar(const SAT: TACBrSAT);
begin
  Self.NumeroSessao := SAT.Resposta.numeroSessao;
  Self.CodigoDeRetorno  := SAT.Resposta.codigoDeRetorno;
  Self.RetornoStr  := SAT.Resposta.RetornoStr;
  Self.XML:= SAT.CFeCanc.AsXMLString;

  if (SAT.CFeCanc.NomeArquivo <> '') and FileExists(SAT.CFeCanc.NomeArquivo) then
    Self.Arquivo:= SAT.CFeCanc.NomeArquivo;
end;

{ TRetornoCriarCFe }
constructor TRetornoCriarCFe.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoCFe, ATipo, AFormato);
end;

procedure TRetornoCriarCFe.Processar(const SAT: TACBrSAT);
begin
  Self.nCFe := IntToStr(SAT.CFe.ide.nCFe);
  Self.XML  := SAT.CFe.AsXMLString;
  Self.Arquivo := SAT.CalcCFeNomeArq(SAT.ConfigArquivos.PastaEnvio,
                          IntToStrZero(SAT.CFe.ide.numeroCaixa,3)+'-'+
                          IntToStrZero(SAT.CFe.ide.cNF,6),'-satcfe');

  SAT.CFe.SaveToFile(Arquivo);
end;

{ TRetornoConsultarSessaoCancelado }
constructor TRetornoConsultarSessaoCancelado.Create(
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoCFeCancelado, ATipo, AFormato);
end;

procedure TRetornoConsultarSessaoCancelado.Processar(const SAT: TACBrSAT);
begin
  with SAT.CFeCanc do
  begin
    Self.nCFeCanc := IntToStrZero(ide.nCFe, 0);
    Self.XML := AsXMLString;
    Self.Arquivo := SAT.CFeCanc.NomeArquivo;
  end;
end;

{ TPadraoResposta }
constructor TPadraoSATResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRetornoConsultarSessao }
constructor TRetornoConsultarSessao.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoCFe, ATipo, AFormato);
end;

procedure TRetornoConsultarSessao.Processar(const SAT: TACBrSAT);
begin
  with SAT.CFe do
  begin
    Self.nCFe := IntToStrZero(ide.nCFe, 0);
    Self.XML := AsXMLString;
    Self.Arquivo := SAT.CFe.NomeArquivo;
  end;
end;

{ TPadraoResposta }
constructor TACBrLibSATResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespostaSat, ATipo, AFormato);
end;

procedure TACBrLibSATResposta.Processar(const SAT: TACBrSAT);
begin
  with SAT.Resposta do
  begin
    Self.NumeroSessao := numeroSessao;
    Self.CodigoDeRetorno := codigoDeRetorno;
    Self.CodigoDeErro := codigoDeErro;
    Self.MensagemRetorno := mensagemRetorno;
    Self.CodigoSEFAZ := codigoSEFAZ;
    Self.MensagemSEFAZ := mensagemSEFAZ;
    Self.Retorno := RetornoStr;
  end;
end;

end.

