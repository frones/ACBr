{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{ Colaboradores neste arquivo: Italo Jurisato Junior                           }
{																			   }
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

unit pcnAdmCSCNFCe;

interface

uses
  SysUtils, Classes,
  ACBrDFeConsts,
  pcnNFeConsts,
  pcnConversao, pcnGerador;

type

  TAdmCSCNFCe = class(TObject)
  private
    FGerador: TGerador;
    FVersao: String;
    FtpAmb: TpcnTipoAmbiente;
    FindOP: TpcnIndOperacao;
    FraizCNPJ: String;
    FidCsc: Integer;
    FcodigoCsc: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: boolean;
    function ObterNomeArquivo: String;
    property Gerador: TGerador       read FGerador   write FGerador;
    property Versao: String          read FVersao    write FVersao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb     write FtpAmb;
    property indOP: TpcnIndOperacao  read FindOP     write FindOP;
    property raizCNPJ: String        read FraizCNPJ  write FraizCNPJ;
    property idCsc: Integer          read FidCsc     write FidCsc;
    property codigoCsc: String       read FcodigoCsc write FcodigoCsc;
  end;

const
  DSC_INDOP    = 'Indicador de Operação';
  DSC_RAIZCNPJ = 'Raiz do CNPJ';
  DSC_IDCSC    = 'Identificador do CSC';
  DSC_CODCSC   = 'Código do CSC';

implementation

Uses pcnAuxiliar;

{ TAdmCSCNFCe }

constructor TAdmCSCNFCe.Create;
begin
  inherited Create;
  FGerador := TGerador.Create;
end;

destructor TAdmCSCNFCe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TAdmCSCNFCe.ObterNomeArquivo: String;
var
  DataHora: TDateTime;
  Year, Month, Day, Hour, Min, Sec, Milli: Word;
  AAAAMMDDTHHMMSS: String;
begin
  Datahora:=now;
  DecodeTime(DataHora, Hour, Min, Sec, Milli);
  DecodeDate(DataHora, Year, Month, Day);
  AAAAMMDDTHHMMSS := IntToStrZero(Year, 4) + IntToStrZero(Month, 2) + IntToStrZero(Day, 2) +
    IntToStrZero(Hour, 2) + IntToStrZero(Min, 2) + IntToStrZero(Sec, 2);
  Result := AAAAMMDDTHHMMSS + '-ped-csc.xml';
end;

function TAdmCSCNFCe.GerarXML: boolean;
begin
  Gerador.ArquivoFormatoXML := '';
  Gerador.wGrupo('admCscNFCe ' + NAME_SPACE + ' versao="' + Versao + '"');
  Gerador.wCampo(tcStr, 'AP03', 'tpAmb   ', 01, 01, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, 'AP04', 'indOp   ', 01, 01, 1, IndOperacaoToStr(FindOp), DSC_INDOP);
  Gerador.wCampo(tcStr, 'AP05', 'raizCNPJ', 08, 08, 1, FraizCNPJ, DSC_RAIZCNPJ);

  if FindOp = ioRevogaCSC then
  begin
    Gerador.wGrupo('dadosCsc');
    Gerador.wCampo(tcStr, 'AP07', 'idCsc    ', 06, 06, 1, FormatFloat('000000', FidCsc), DSC_IDCSC);
    Gerador.wCampo(tcStr, 'AP08', 'codigoCsc', 16, 16, 1, FcodigoCsc, DSC_CODCSC);
    Gerador.wGrupo('/dadosCsc');
  end;

  Gerador.wGrupo('/admCscNFCe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

