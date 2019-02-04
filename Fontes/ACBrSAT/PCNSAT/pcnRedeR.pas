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

unit pcnRedeR;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, pcnRede;

type

{ TCFeR }

  TRedeR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FRede: TRede;
  public
    constructor Create(AOwner: TRede);
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Rede: TRede read FRede write FRede;
  end;

implementation

uses ACBrConsts;

{ TCFeR }

constructor TRedeR.Create(AOwner: TRede);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FRede := AOwner;
end;

destructor TRedeR.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TRedeR.LerXml: boolean;
var
  ok: boolean;
begin
  Result := False;
  Rede.Clear;
  ok := true;

  if Leitor.rExtrai(1, 'config') <> '' then
  begin
    Rede.tipoInter  := StrToTipoInterface(ok, Leitor.rCampo(tcStr, 'tipoInter'));
    Rede.SSID       := Leitor.rCampo(tcStr, 'SSID');
    Rede.seg        := StrToSegSemFio(ok, Leitor.rCampo(tcStr, 'sef'));
    Rede.codigo     := Leitor.rCampo(tcStr, 'codigo');
    Rede.tipoLan    := StrToTipoLan(ok, Leitor.rCampo(tcStr, 'tipoLan'));
    Rede.lanIP      := Leitor.rCampo(tcStr, 'lanIP');
    Rede.lanMask    := Leitor.rCampo(tcStr, 'lanMask');
    Rede.lanGW      := Leitor.rCampo(tcStr, 'lanGW');
    Rede.lanDNS1    := Leitor.rCampo(tcStr, 'lanDNS1');
    Rede.lanDNS2    := Leitor.rCampo(tcStr, 'lanDNS2');
    Rede.usuario    := Leitor.rCampo(tcStr, 'usuario');
    Rede.senha      := Leitor.rCampo(tcStr, 'senha');
    Rede.proxy      := Leitor.rCampo(tcInt, 'proxy');
    Rede.proxy_ip   := Leitor.rCampo(tcStr, 'proxy_ip');
    Rede.proxy_porta:= Leitor.rCampo(tcInt, 'proxy_porta');
    Rede.proxy_user := Leitor.rCampo(tcStr, 'proxy_user');
    Rede.proxy_senha:= Leitor.rCampo(tcStr, 'proxy_senha');
  end;

  Result := True;
end;

end.
