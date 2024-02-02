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

unit ACBrDFeDistDFeInt;

interface

uses
  SysUtils, Classes, pcnConversao, pcnGerador;

type

  { TDistDFeInt }

  TDistDFeInt = class
  private
    FGerador: TGerador;
    FtpAmb: TpcnTipoAmbiente;
    FcUFAutor: Integer;
    FCNPJCPF: String;
    FultNSU: String;
    FNSU: String;
    FChave: String;

    FVersao: String;
    FNameSpace: String;
    FtagGrupoMsg: String;
    FtagconsChDFe: String;
    FtagchDFe: String;
    FGerarcUFAutor: Boolean;
  public
    constructor Create(const AVersao, ANameSpace, AtagGrupoMsg, AtagconsChDFe, AtagchDFe: String; AGerarcUFAutor: Boolean);
    destructor Destroy; override;
    function GerarXML: boolean;
    function ObterNomeArquivo: string;
    property Gerador: TGerador       read FGerador  write FGerador;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property cUFAutor: Integer       read FcUFAutor write FcUFAutor;
    property CNPJCPF: String         read FCNPJCPF  write FCNPJCPF;
    property ultNSU: String          read FultNSU   write FultNSU;
    // Usado no Grupo de informações para consultar um DF-e a partir de um
    // NSU específico.
    property NSU: String             read FNSU      write FNSU;
    // Usado no Grupo de informações para consultar um DF-e a partir de uma
    // chave específica.
    property Chave: String           read FChave    write FChave;
  end;

implementation

uses
  ACBrDFeConsts,
  ACBrNF3eConsts,
  pcnAuxiliar;

{ TDistDFeInt }

constructor TDistDFeInt.Create(const AVersao, ANameSpace, AtagGrupoMsg,
  AtagconsChDFe, AtagchDFe: String; AGerarcUFAutor: Boolean);
begin
  FGerador := TGerador.Create;
  FVersao := AVersao;
  FNameSpace := ANameSpace;
  FtagGrupoMsg := AtagGrupoMsg;
  FtagconsChDFe := AtagconsChDFe;
  FtagchDFe := AtagchDFe;
  FGerarcUFAutor := AGerarcUFAutor;
end;

destructor TDistDFeInt.Destroy;
begin
  FGerador.Free;

  inherited;
end;

function TDistDFeInt.ObterNomeArquivo: string;
var
  DataHora: TDateTime;
  Year, Month, Day, Hour, Min, Sec, Milli: Word;
  AAAAMMDDTHHMMSS: string;
begin
  Datahora := now;
  DecodeTime(DataHora, Hour, Min, Sec, Milli);
  DecodeDate(DataHora, Year, Month, Day);
  AAAAMMDDTHHMMSS := IntToStrZero(Year, 4) + IntToStrZero(Month, 2) + IntToStrZero(Day, 2) +
    IntToStrZero(Hour, 2) + IntToStrZero(Min, 2) + IntToStrZero(Sec, 2);
  Result := AAAAMMDDTHHMMSS + '-con-dist-dfe.xml';
end;

function TDistDFeInt.GerarXML: boolean;
var
 sNSU: String;
begin
  Gerador.ArquivoFormatoXML := '';

  if FtagGrupoMsg <> '' then
    Gerador.wGrupo(FtagGrupoMsg);

  Gerador.wGrupo('distDFeInt ' + FNameSpace + ' versao="' + FVersao + '"');
  Gerador.wCampo(tcStr, 'A03', 'tpAmb   ', 01, 01, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);

  if FGerarcUFAutor then
    Gerador.wCampo(tcInt, 'A04', 'cUFAutor', 02, 02, 0, FcUFAutor, DSC_UF);

  Gerador.wCampoCNPJCPF('A05', 'A06', FCNPJCPF);

  if FNSU = '' then
  begin
    if FChave = '' then
    begin
      sNSU := IntToStrZero(StrToIntDef(FultNSU,0),15);
      Gerador.wGrupo('distNSU');
      Gerador.wCampo(tcStr, 'A08', 'ultNSU', 01, 15, 1, sNSU, DSC_ULTNSU);
      Gerador.wGrupo('/distNSU');
    end
    else begin
      Gerador.wGrupo(FtagconsChDFe);
      Gerador.wCampo(tcStr, 'A12', FtagchDFe, 44, 44, 1, FChave, DSC_CHAVE);

      if not ValidarChave(FChave) then
        Gerador.wAlerta('A12', FtagchDFe, '', 'Chave do DFe inválida');

      Gerador.wGrupo('/' + FtagconsChDFe);
    end;
  end
  else
  begin
    sNSU := IntToStrZero(StrToIntDef(FNSU,0),15);
    Gerador.wGrupo('consNSU');
    Gerador.wCampo(tcStr, 'A10', 'NSU', 01, 15, 1, sNSU, DSC_NSU);
    Gerador.wGrupo('/consNSU');
  end;

  Gerador.wGrupo('/distDFeInt');

  if FtagGrupoMsg <> '' then
    Gerador.wGrupo('/' + FtagGrupoMsg);

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

