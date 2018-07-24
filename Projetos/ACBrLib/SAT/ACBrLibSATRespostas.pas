{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibSATRespostas;

interface

uses
  SysUtils, Classes, ACBrLibResposta;

type

  { TPadraoSATResposta }

  TPadraoSATResposta = class(TACBrLibResposta)
  private
    FArquivo: String;
    FXML: String;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Arquivo: String read FArquivo write FArquivo;
    property XML: String read FXML write FXML;

  end;

  { TRetornoConsultarSessao }

  TRetornoConsultarSessao = class(TPadraoSATResposta)
  private
    FnCFe: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property nCFe: String read FnCFe write FnCFe;

  end;

  { TRetornoConsultarSessaoCancelado }

  TRetornoConsultarSessaoCancelado = class(TPadraoSATResposta)
  private
    FnCFeCanc: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property nCFeCanc: String read FnCFeCanc write FnCFeCanc;


  end;

  { TRetornoCriarCFe }

  TRetornoCriarCFe = class(TPadraoSATResposta)
  private
    FnCFe: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

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
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

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
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

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
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

  published
    property Resultado: String read FResultado write FResultado;
    property NumeroSessao: Integer read FnumeroSessao write FnumeroSessao;
    property CodigoDeRetorno: Integer read FCodigoDeRetorno write FCodigoDeRetorno;
    property RetornoStr: String read FRetornoStr write FRetornoStr;

  end;

  { TRetornoStatusSAT }

  TRetornoStatusSAT = class(TACBrLibResposta)
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
    FULTIMO_CFe      : Integer;
    FLISTA_INICIAL   : Integer;
    FLISTA_FINAL     : Integer;
    FDH_CFe          : TDateTime;
    FDH_ULTIMA       : TDateTime;
    FCERT_EMISSAO    : String;
    FCERT_VENCIMENTO : TDateTime;
    FESTADO_OPERACAO : String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo); reintroduce;

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
    property ULTIMO_CFe       : Integer   read  FULTIMO_CFe       write FULTIMO_CFe;
    property LISTA_INICIAL    : Integer   read  FLISTA_INICIAL    write FLISTA_INICIAL;
    property LISTA_FINAL      : Integer   read  FLISTA_FINAL      write FLISTA_FINAL;
    property DH_CFe           : TDateTime read  FDH_CFe           write FDH_CFe;
    property DH_ULTIMA        : TDateTime read  FDH_ULTIMA        write FDH_ULTIMA;
    property CERT_EMISSAO     : String    read  FCERT_EMISSAO     write FCERT_EMISSAO;
    property CERT_VENCIMENTO  : TDateTime read  FCERT_VENCIMENTO  write FCERT_VENCIMENTO;
    property ESTADO_OPERACAO  : String    read  FESTADO_OPERACAO  write FESTADO_OPERACAO;


  end;


implementation

uses
  ACBrLibSATConsts;

{ TRetornoStatusSAT }

constructor TRetornoStatusSAT.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoStatusSAT, ATipo);
end;

{ TRetornoEnvio }

constructor TRetornoEnvio.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoENVIO, ATipo);
end;

{ TRetornoTesteFimaFim }

constructor TRetornoTesteFimaFim.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoTESTEFIMAFIM, ATipo);
end;

{ TRetornoCancelarCFe }

constructor TRetornoCancelarCFe.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoCFeCancelado, ATipo);
end;

{ TRetornoCriarCFe }

constructor TRetornoCriarCFe.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoCFe, ATipo);
end;

{ TRetornoConsultarSessaoCancelado }

constructor TRetornoConsultarSessaoCancelado.Create(
  const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoCFeCancelado, ATipo);
end;

{ TPadraoResposta }

constructor TPadraoSATResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(ASessao, ATipo);
end;

{ TRetornoConsultarSessao }

constructor TRetornoConsultarSessao.Create(const ATipo: TACBrLibRespostaTipo);
begin
  inherited Create(CSessaoCFe, ATipo);
end;


end.

