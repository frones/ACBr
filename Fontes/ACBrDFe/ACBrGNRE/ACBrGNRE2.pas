{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit ACBrGNRE2;

interface

uses
  Classes, Sysutils,
  pgnreGNRE,
  pgnreConversao,
{$IFDEF CLX}
  QDialogs,
{$ELSE}
  Dialogs,
{$ENDIF}
  ACBrGNREGuias,
  ACBrGNREGuiasRetorno,
  ACBrGNREWebServices,
  ACBrGNREConfiguracoes,
  ACBrGNREGuiaClass,
  ACBrUtil, ACBrGNREUtil;

const
  ACBrGNRE_VERSAO = '0.1.0';

type
  TACBrGNREAboutInfo = (ACBrGNREAbout);

  // Evento para gerar log das mensagens do Componente
  TACBrGNRELog = procedure(const Mensagem : String) of object ;

  TACBrGNRE = class(TComponent)
  private
    fsAbout: TACBrGNREAboutInfo;
    FGuias: TGuias;
    FGuiasRetorno: TGuiasRetorno;
    FGNREGuia: TACBrGNREGuiaClass;
    FWebServices: TWebServices;
    FConfiguracoes: TConfiguracoes;
    FStatus : TStatusACBrGNRE;
    FOnStatusChange: TNotifyEvent;
    FOnGerarLog : TACBrGNRELog;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Enviar: Boolean;
    function ConsultarResultadoLote(ANumRecibo: String): Boolean;

    property WebServices: TWebServices   read FWebServices  write FWebServices;
    property Guias: TGuias read FGuias write FGuias;
    property GuiasRetorno: TGuiasRetorno read FGuiasRetorno write FGuiasRetorno;
    property Status: TStatusACBrGNRE     read FStatus;
    procedure SetStatus( const stNewStatus : TStatusACBrGNRE );
  published
    property Configuracoes: TConfiguracoes     read FConfiguracoes  write FConfiguracoes;
    property OnStatusChange: TNotifyEvent      read FOnStatusChange write FOnStatusChange;
    property GNREGuia: TACBrGNREGuiaClass      read FGNREGuia       write FGNREGuia;
    property AboutACBrGNRE: TACBrGNREAboutInfo read fsAbout         write fsAbout stored false;
    property OnGerarLog: TACBrGNRELog          read FOnGerarLog     write FOnGerarLog;
  end;

procedure ACBrAboutDialog;

implementation

procedure ACBrAboutDialog;
var
 Msg: String;
begin
 Msg := 'Componente ACBrGNRE'+#10+
        'Versão: '+ACBrGNRE_VERSAO+#10+#10+
        'Automação Comercial Brasil'+#10+#10+
        'http://acbr.sourceforge.net'+#10+#10+
        'Projeto Cooperar - PCN'+#10+#10+
        'http://www.projetocooperar.org/pcn/';

 MessageDlg(Msg ,mtInformation ,[mbOk],0);
end;

{ TACBrGNRE }

procedure TACBrGNRE.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent, Operation);
end;

constructor TACBrGNRE.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FConfiguracoes      := TConfiguracoes.Create( self );
 FConfiguracoes.Name := 'Configuracoes';

 {$IFDEF COMPILER6_UP}
   FConfiguracoes.SetSubComponent( true ); { para gravar no DFM/XFM }
 {$ENDIF}

 FGuias               := TGuias.Create(Self, Guia);
 FGuiasRetorno        := TGuiasRetorno.Create(Self, GuiaRetorno);
 FGuias.Configuracoes := FConfiguracoes;
 FWebServices         := TWebServices.Create(Self);

 if FConfiguracoes.WebServices.Tentativas <= 0
  then FConfiguracoes.WebServices.Tentativas := 18;

 {$IFDEF ACBrGNREOpenSSL}
   GNREUtil.InitXmlSec;
 {$ENDIF}

 FOnGerarLog := nil;
end;

destructor TACBrGNRE.Destroy;
begin
 FConfiguracoes.Free;
 FGuias.Free;
 FGuiasRetorno.Free;
 FWebServices.Free;

 {$IFDEF ACBrGNREOpenSSL}
    GNREUtil.ShutDownXmlSec;
 {$ENDIF}

 inherited destroy;
end;

function TACBrGNRE.Enviar: Boolean;
begin
  if Guias.Count <= 0 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Nenhuma GNRE adicionado');
    raise Exception.Create('ERRO: Nenhuma GNRE adicionado');
    exit;
  end;

  if Guias.Count > 50 then
  begin
    if Assigned(Self.OnGerarLog) then
      Self.OnGerarLog('ERRO: Conjunto de GNRE transmitidos (máximo de 50) excedido. Quantidade atual: '+IntToStr(Guias.Count));
    raise Exception.Create('ERRO: Conjunto de GNRE transmitidos (máximo de 50) excedido. Quantidade atual: '+IntToStr(Guias.Count));
    exit;
  end;

  Result := WebServices.Envia;
end;

function TACBrGNRE.ConsultarResultadoLote(ANumRecibo: String): Boolean;
begin
 Result := WebServices.ConsultaResultadoLote(ANumRecibo);
end;

procedure TACBrGNRE.SetStatus( const stNewStatus : TStatusACBrGNRE );
begin
 if ( stNewStatus <> FStatus )
  then begin
   FStatus := stNewStatus;
   if Assigned(fOnStatusChange)
    then FOnStatusChange(Self);
  end;
end;

end.
