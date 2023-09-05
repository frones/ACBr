{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Arimateia Jr - https://nuvemfiscal.com.br       }
{                              Victor H. Gonzales - Pandaaa                    }
{                                                                              }
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

unit ACBrNFeUtilsFPDF;

interface

uses
  Classes,
  SysUtils,
  StrUtils,

  pcnNFe,
  pcnConversao,
  ACBrNFe,
  ACBrNFeDANFEClass,
  StrUtilsEx;

type
  TLogoAlign = (laLeft, laCenter, laRight, laFull);

  TPosRecibo = (prCabecalho, prRodape, prEsquerda);

  TNFeUtilsFPDF = class
  private
    FDANFEClassOwner: TACBrNFeDANFEClass;
    FNFe: TNFe;
    FFormatSettings: TFormatSettings;
  public
    function NotaCancelada: boolean;
    function NotaDenegada: boolean;
    function NotaEPEC: boolean;
    function GetChaveContingencia: string;
    function GetTextoFatura: string;
    function GetTextoAdicional: string;
  public
    property DANFEClassOwner: TACBrNFeDANFEClass read FDANFEClassOwner;
    property NFe: TNFe read FNFe;
  public
    constructor Create(ANFe: TNFe);
    destructor Destroy; override;
    property FormatSettings: TFormatSettings read FFormatSettings write FFormatSettings;
  end;

implementation

{ TNFeUtilsFPDF }

constructor TNFeUtilsFPDF.Create(ANFe: TNFe);
begin
  FNFe := ANFe;
  FDANFEClassOwner := TACBrNFeDANFEClass.Create(nil);
end;

destructor TNFeUtilsFPDF.Destroy;
begin
  FDANFEClassOwner.Free;
  inherited;
end;

function TNFeUtilsFPDF.GetChaveContingencia: string;
var
  ACBrNFe: TACBrNFe;
begin
  ACBrNFe := TACBrNFe.Create(nil);
  try
    Result := ACBrNFe.GerarChaveContingencia(NFe);
  finally
    ACBrNFe.Free;
  end;
end;

function TNFeUtilsFPDF.GetTextoAdicional: string;
begin
  Result := DANFEClassOwner.ManterInformacoesDadosAdicionais(NFe);
  Result := FastStringReplace(Result, ';', sLineBreak, [rfReplaceAll]);
end;

function TNFeUtilsFPDF.GetTextoFatura: string;
var
  textoIndPag: string;
  Separador: string;
begin
  if NFe.Cobr.Fat.nFat = '' then
    result := '';

  textoIndPag := '';
  if NFe.Ide.indPag = ipVista then
    textoIndPag := 'Pagamento à Vista - '
  else if NFe.Ide.indPag = ipPrazo then
    textoIndPag := 'Pagamento à Prazo - ';

  Separador := '   -   ';

  Result :=
    textoIndPag +
    IfThen(NFe.Cobr.Fat.nFat <> '', Format('Fatura: %s%s', [NFe.Cobr.Fat.nFat, separador])) +
    FormatFloat('Valor Original: R$ #,0.00', NFe.Cobr.Fat.vOrig, FFormatSettings) + separador +
    FormatFloat('Desconto: R$ #,0.00', NFe.Cobr.Fat.vDesc, FFormatSettings) + separador +
    FormatFloat('Valor Líquido: R$ #,0.00', NFe.Cobr.Fat.vLiq, FFormatSettings);
end;

function TNFeUtilsFPDF.NotaCancelada: boolean;
var
  cStat: integer;
begin
  cStat := NFe.procNFe.cStat;
  Result :=
    (cStat = 101) or
    (cStat = 151) or
    (cStat = 155);
end;

function TNFeUtilsFPDF.NotaDenegada: boolean;
var
  cStat: integer;
begin
  cStat := NFe.procNFe.cStat;
  Result :=
    (cStat = 110) or
    (cStat = 301) or
    (cStat = 302) or
    (cStat = 303);
end;

function TNFeUtilsFPDF.NotaEPEC: boolean;
begin
  Result := NFe.Ide.tpEmis = teDPEC;
end;

end.
