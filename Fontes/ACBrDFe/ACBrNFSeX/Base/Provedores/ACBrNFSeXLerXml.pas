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
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlReader,
  ACBrNFSeXInterface, ACBrNFSeXClass, ACBrNFSeXConversao;

type
  { TNFSeRClass }

  TNFSeRClass = class(TACBrXmlReader)
  private
    FNFSe: TNFSe;
    FProvedor: TnfseProvedor;
    FtpXML: TtpXML;
    FAmbiente: TACBrTipoAmbiente;

  protected
    FpAOwner: IACBrNFSeXProvider;

    function NormatizarItemListaServico(const Codigo: string): string;
    function ItemListaServicoDescricao(const Codigo: string): string;
    function TipodeXMLLeitura(const aArquivo: string): TtpXML; virtual;
    function NormatizarXml(const aXml: string): string; virtual;
    function NormatizarAliquota(const Aliquota: Double): Double;
  public
    constructor Create(AOwner: IACBrNFSeXProvider);

    function LerXml: Boolean; Override;

    property NFSe: TNFSe             read FNFSe     write FNFSe;
    property Provedor: TnfseProvedor read FProvedor write FProvedor;
    property tpXML: TtpXML           read FtpXML    write FtpXML;
    property Ambiente: TACBrTipoAmbiente read FAmbiente write FAmbiente default taHomologacao;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrDFeException;

{ TNFSeRClass }

constructor TNFSeRClass.Create(AOwner: IACBrNFSeXProvider);
begin
  FpAOwner := AOwner;
end;

function TNFSeRClass.ItemListaServicoDescricao(const Codigo: string): string;
var
  xCodigo: string;
begin
  xCodigo := OnlyNumber(Codigo);

  if FpAOwner.ConfigGeral.TabServicosExt then
    Result := ObterDescricaoServico(xCodigo)
  else
    Result := CodItemServToDesc(xCodigo);
end;

function TNFSeRClass.LerXml: Boolean;
begin
  Result := False;
  raise EACBrDFeException.Create(ClassName + '.LerXml, não implementado');
end;

function TNFSeRClass.NormatizarAliquota(const Aliquota: Double): Double;
begin
  if Aliquota < 1 then
    Result := Aliquota * 100
  else
    Result := Aliquota;
end;

function TNFSeRClass.NormatizarItemListaServico(const Codigo: string): string;
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

function TNFSeRClass.NormatizarXml(const aXml: string): string;
begin
  Result := TiraAcentos(aXml);
end;

function TNFSeRClass.TipodeXMLLeitura(const aArquivo: string): TtpXML;
begin
  if (Pos('/infnfse>', LowerCase(Arquivo)) > 0) then
    Result := txmlNFSe
  else
    Result := txmlRPS;
end;

end.
