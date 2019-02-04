{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esse arquivo usa a classe  PCN (c) 2009 - Paulo Casagrande                  }
{  PCN - Projeto Cooperar NFe       (Found at URL:  www.projetocooperar.org)   }
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

{$I ACBr.inc}

unit pcnRede;

interface

uses
  SysUtils, Classes,
  pcnConversao;

type

  TTipoInterface = (infETHE, infWIFI);
  TSegSemFio = (segNONE, segWEP, segWPA, segWPA2, segWPA_PERSONAL, segWPA_ENTERPRISE);
  TTipoLan = (lanDHCP, lanPPPoE, lanIPFIX);

  { TRede }

  TRede = class(TPersistent)
  private
    Fcodigo: String;
    FlanDNS1: String;
    FlanDNS2: String;
    FlanGW: String;
    FlanIP: String;
    FlanMask: String;
    Fproxy: Integer;
    Fproxy_ip: String;
    Fproxy_porta: Integer;
    Fproxy_senha: String;
    Fproxy_user: String;
    Fseg: TSegSemFio;
    Fsenha: String;
    FSSID: String;
    FtipoInter : TTipoInterface;
    FtipoLan: TTipoLan;
    Fusuario: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function LoadFromFile(const AFileName : String): boolean;
    function SaveToFile(const AFileName : String; ApenasTagsAplicacao: Boolean = false): boolean;
    function GetXMLString: AnsiString;
    procedure SetXMLString(const AValue : AnsiString);

    property AsXMLString : AnsiString read GetXMLString write SetXMLString;
  published
    property tipoInter: TTipoInterface read FtipoInter write FtipoInter;
    property SSID: String read FSSID write FSSID;
    property seg: TSegSemFio read Fseg write Fseg;
    property codigo: String read Fcodigo write Fcodigo;
    property tipoLan: TTipoLan read FtipoLan write FtipoLan;
    property lanIP: String read FlanIP write FlanIP;
    property lanMask: String read FlanMask write FlanMask;
    property lanGW: String read FlanGW write FlanGW;
    property lanDNS1: String read FlanDNS1 write FlanDNS1;
    property lanDNS2: String read FlanDNS2 write FlanDNS2;
    property usuario: String read Fusuario write Fusuario;
    property senha: String read Fsenha write Fsenha;
    property proxy: Integer read Fproxy write Fproxy;
    property proxy_ip: String read Fproxy_ip write Fproxy_ip;
    property proxy_porta: Integer read Fproxy_porta write Fproxy_porta;
    property proxy_user: String read Fproxy_user write Fproxy_user;
    property proxy_senha: String read Fproxy_senha write Fproxy_senha;
  end;

function TipoInterfaceToStr(const t: TTipoInterface ): string;
function StrToTipoInterface(var ok: boolean; const s: string): TTipoInterface;
function SegSemFioToStr(const t: TSegSemFio ): string;
function StrToSegSemFio(var ok: boolean; const s: string): TSegSemFio;
function TipoLanToStr(const t: TTipoLan ): string;
function StrToTipoLan(var ok: boolean; const s: string): TTipoLan;

implementation

uses
  dateutils, pcnRedeW, pcnRedeR;

function TipoInterfaceToStr(const t: TTipoInterface ): string;
begin
  result := EnumeradoToStr(t, ['ETHE', 'WIFI'], [infETHE, infWIFI]);
end;

function StrToTipoInterface(var ok: boolean; const s: string): TTipoInterface;
begin
  result := StrToEnumerado(ok, s, ['ETHE', 'WIFI'], [infETHE, infWIFI]);
end;

function SegSemFioToStr(const t: TSegSemFio ): string;
begin
  result := EnumeradoToStr(t, ['', 'WEP', 'WPA', 'WPA2', 'WPAPERSONAL', 'WPAENTERPRISE'],
                              [segNONE, segWEP, segWPA, segWPA2, segWPA_PERSONAL, segWPA_ENTERPRISE]);
end;

function StrToSegSemFio(var ok: boolean; const s: string): TSegSemFio;
begin
  result := StrToEnumerado(ok, s, ['', 'WEP', 'WPA', 'WPA2', 'WPAPERSONAL', 'WPAENTERPRISE'],
                              [segNONE, segWEP, segWPA, segWPA2, segWPA_PERSONAL, segWPA_ENTERPRISE]);
end;

function TipoLanToStr(const t: TTipoLan ): string;
begin
  result := EnumeradoToStr(t, ['DHCP', 'PPPoE', 'IPFIX'], [lanDHCP, lanPPPoE, lanIPFIX]);
end;

function StrToTipoLan(var ok: boolean; const s: string): TTipoLan;
begin
  result := StrToEnumerado(ok, s, ['DHCP', 'PPPoE', 'IPFIX'], [lanDHCP, lanPPPoE, lanIPFIX]);
end;

{ TRede }

constructor TRede.Create;
begin
  inherited Create;
  Clear;
end;

destructor TRede.Destroy;
begin
  inherited Destroy;
end;

procedure TRede.Clear;
begin
  Fcodigo     := '';
  FlanDNS1    := '';
  FlanDNS2    := '';
  FlanGW      := '';
  FlanIP      := '';
  FlanMask    := '';
  Fproxy      := 0;
  Fproxy_ip   := '';
  Fproxy_porta:= 0;
  Fproxy_senha:= '';
  Fproxy_user := '';
  Fseg        := segNONE;
  FSSID       := '';
  FtipoInter  := infETHE;
  FtipoLan    := lanDHCP;
  Fusuario    := '';
  Fsenha      := '';
end;

function TRede.LoadFromFile(const AFileName: String): boolean;
var
  SL : TStringList;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.LoadFromFile( AFileName );
    AsXMLString := SL.Text;
    Result      := True;
  finally
    SL.Free;
  end;
end;

function TRede.SaveToFile(const AFileName: String; ApenasTagsAplicacao: Boolean
  ): boolean;
var
  SL : TStringList;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.Text := GetXMLString;
    SL.SaveToFile( AFileName );
    Result := True;
  finally
    SL.Free;
  end;
end;

function TRede.GetXMLString(): AnsiString;
var
  LocRedeW : TRedeW;
begin
  Result  := '';
  LocRedeW := TRedeW.Create(Self);
  try
    LocRedeW.Gerador.Opcoes.IdentarXML := True;
    LocRedeW.Gerador.Opcoes.TamanhoIdentacao := 3;

    LocRedeW.GerarXml();
    Result := LocRedeW.Gerador.ArquivoFormatoXML;
  finally
    LocRedeW.Free;
  end;
end;

procedure TRede.SetXMLString(const AValue: AnsiString);
var
 LocRedeR : TRedeR;
begin
  LocRedeR := TRedeR.Create(Self);
  try
    LocRedeR.Leitor.Arquivo := AValue;
    LocRedeR.LerXml;
  finally
    LocRedeR.Free
  end;
end;

end.
 
