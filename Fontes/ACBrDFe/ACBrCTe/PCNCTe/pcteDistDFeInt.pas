{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{$I ACBr.inc}

unit pcteDistDFeInt;

interface

uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnGerador, pcnConsts;

type

  TDistDFeInt = class(TPersistent)
  private
    FGerador: TGerador;
    FVersao: String;
    FtpAmb: TpcnTipoAmbiente;
    FcUFAutor: Integer;
    FCNPJCPF: String;
    FultNSU: String;
    FNSU: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: boolean;
  published
    property Gerador: TGerador       read FGerador  write FGerador;
    property Versao: String          read FVersao   write FVersao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property cUFAutor: Integer       read FcUFAutor write FcUFAutor;
    property CNPJCPF: String         read FCNPJCPF  write FCNPJCPF;
    property ultNSU: String          read FultNSU   write FultNSU;
    // Usado no Grupo de informações para consultar um DF-e a partir de um
    // NSU específico.
    property NSU: String             read FNSU      write FNSU;
  end;

implementation

{ TDistDFeInt }

constructor TDistDFeInt.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TDistDFeInt.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TDistDFeInt.GerarXML: boolean;
var
 sNSU: String;
begin
  Gerador.ArquivoFormatoXML := '';
  Gerador.wGrupo('cteDadosMsg');
  Gerador.wGrupo('distDFeInt ' + NAME_SPACE_CTE + ' versao="' + Versao + '"');
  Gerador.wCampo(tcStr, 'A03', 'tpAmb   ', 01, 01, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
  Gerador.wCampo(tcInt, 'A04', 'cUFAutor', 02, 02, 1, FcUFAutor, '***');
  Gerador.wCampoCNPJCPF('A05', 'A06', FCNPJCPF);

  if FNSU = '' then
  begin
    sNSU := IntToStrZero(StrToIntDef(FultNSU,0),15);
    Gerador.wGrupo('distNSU');
    Gerador.wCampo(tcStr, 'A08', 'ultNSU', 01, 15, 1, sNSU, DSC_ULTNSU);
    Gerador.wGrupo('/distNSU');
  end
  else
  begin
    sNSU := IntToStrZero(StrToIntDef(FNSU,0),15);
    Gerador.wGrupo('consNSU');
    Gerador.wCampo(tcStr, 'A10', 'NSU', 01, 15, 1, sNSU, DSC_NSU);
    Gerador.wGrupo('/consNSU');
  end;

  Gerador.wGrupo('/distDFeInt');
  Gerador.wGrupo('/cteDadosMsg');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

