{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrNFSeXLerXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes,
  ACBrUtil, ACBrDFeException, ACBrXmlReader, ACBrNFSeXInterface,
  ACBrNFSeXParametros, ACBrNFSeXClass, ACBrNFSeXConversao;

type
  { TNFSeRClass }

  TNFSeRClass = class(TACBrXmlReader)
  private
    FNFSe: TNFSe;
    FtpXML: TtpXML;
    FProvedorConf: TNFSeProvedor;
    FProvedor: TnfseProvedor;

  protected
    FAOwner: IACBrNFSeXProvider;

    function LerDatas(const DataStr: string): TDateTime;
    function NormatizaItemListaServico(const Codigo: string): string;
    function ItemListaServicoDescricao(const Codigo: string): string;
    function TipodeXMLLeitura(const aArquivo: string): TtpXML;
  public
    constructor Create(AOwner: IACBrNFSeXProvider);

    function LerXml: Boolean; Override;

    property NFSe: TNFSe                 read FNFSe         write FNFSe;
    property Provedor: TnfseProvedor     read FProvedor     write FProvedor;
    property ProvedorConf: TNFSeProvedor read FProvedorConf write FProvedorConf;
    property tpXML: TtpXML               read FtpXML        write FtpXML;
  end;

implementation

{ TNFSeRClass }

constructor TNFSeRClass.Create(AOwner: IACBrNFSeXProvider);
begin
  FAOwner := AOwner;
end;

function TNFSeRClass.ItemListaServicoDescricao(const Codigo: string): string;
var
  xCodigo: string;
begin
  xCodigo := OnlyNumber(Codigo);

  if FAOwner.ConfigGeral.TabServicosExt then
    Result := ObterDescricaoServico(xCodigo)
  else
    Result := CodItemServToDesc(xCodigo);
end;

function TNFSeRClass.LerDatas(const DataStr: string): TDateTime;
var
  xData: string;
begin
  xData := Trim(DataStr);

  if xData = '' then
    Result := 0
  else
  begin
    xData := StringReplace(xData, '-', '/', [rfReplaceAll]);

    // Alguns provedores retorna a data de competencia só com o mês e ano
    if Length(xData) = 7 then
    begin
      if Pos('/', xData) = 3 then
        xData := '01/' + xData
      else
        xData := xData + '/01';
    end;

    if (Length(xData) >= 16) and CharInSet(xData[11], ['T', ' ']) then
    begin
      if Pos('/', xData) = 5 then
        // Le a data/hora no formato YYYY/MM/DDTHH:MM:SS
        Result := EncodeDate(StrToInt(copy(xData, 1, 4)),
                             StrToInt(copy(xData, 6, 2)),
                             StrToInt(copy(xData, 9, 2))) +
                  EncodeTime(StrToIntDef(copy(xData, 12, 2), 0),
                             StrToIntDef(copy(xData, 15, 2), 0),
                             StrToIntDef(copy(xData, 18, 2), 0),
                             0)
      else
        // Le a data/hora no formato DD/MM/YYYYTHH:MM:SS
        Result := EncodeDate(StrToInt(copy(xData, 7, 4)),
                             StrToInt(copy(xData, 4, 2)),
                             StrToInt(copy(xData, 1, 2))) +
                  EncodeTime(StrToIntDef(copy(xData, 12, 2), 0),
                             StrToIntDef(copy(xData, 15, 2), 0),
                             StrToIntDef(copy(xData, 18, 2), 0),
                             0)
    end
    else
    begin
      if Pos('/', xData) = 5 then
        // Le a data no formato YYYY/MM/DD
        Result := EncodeDate(StrToInt(copy(xData, 1, 4)),
                             StrToInt(copy(xData, 6, 2)),
                             StrToInt(copy(xData, 9, 2)))
      else
        // Le a data no formato DD/MM/YYYY
        Result := EncodeDate(StrToInt(copy(xData, 7, 4)),
                             StrToInt(copy(xData, 4, 2)),
                             StrToInt(copy(xData, 1, 2)));
    end;
  end;
end;

function TNFSeRClass.LerXml: Boolean;
begin
  Result := False;
  raise EACBrDFeException.Create(ClassName + '.LerXml, não implementado');
end;

function TNFSeRClass.NormatizaItemListaServico(const Codigo: string): string;
var
  Item: Integer;
  xCodigo: string;
begin
  xCodigo := Codigo;

  Item := StrToIntDef(OnlyNumber(xCodigo), 0);
  if Item < 100 then
    Item := Item * 100 + 1;

  xCodigo := FormatFloat('0000', Item);

  Result := Copy(xCodigo, 1, 2) + '.' + Copy(xCodigo, 3, 2);
end;

function TNFSeRClass.TipodeXMLLeitura(const aArquivo: string): TtpXML;
begin
  if (Pos('CompNfse', Arquivo) > 0) or (Pos('ComplNfse', Arquivo) > 0) or
     (Pos('tcCompNfse', Arquivo) > 0) then
    Result := txmlNFSe
  else
    Result := txmlRPS;
end;

end.
