{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010                                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrXmlReader;

interface

uses
  Classes, SysUtils,
  pcnConversao,
  ACBrXmlDocument;

type
  TACBrXmlReader = class
  private
    FArquivo: String;

  protected
    FDocument: TACBrXmlDocument;

  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean; virtual; abstract;
    function CarregarArquivo(const CaminhoArquivo: string): boolean; overload;
    function CarregarArquivo(const Stream: TStringStream): boolean; overload;
    function ProcessarCNPJCPF(const ANode: TACBrXmlNode): string;
    function ProcessarConteudo(const ANode: TACBrXmlNode; const Tipo: TpcnTipoCampo): variant;

    property Document: TACBrXmlDocument read FDocument;
    property Arquivo: String read FArquivo write FArquivo;

  end;

implementation

uses
  ACBrUtil;

{ TACBrXmlReader }
constructor TACBrXmlReader.Create;
begin
  FDocument := TACBrXmlDocument.Create();
end;

destructor TACBrXmlReader.Destroy;
begin
  if FDocument <> nil then FDocument.Free;
  inherited Destroy;
end;

function TACBrXmlReader.CarregarArquivo(const CaminhoArquivo: string): boolean;
var
  ArquivoXML: TStringList;
begin
  //NOTA: Carrega o arquivo xml na memória para posterior leitura de sua tag's
  ArquivoXML := TStringList.Create;
  try
    ArquivoXML.LoadFromFile(CaminhoArquivo);
    FArquivo := ArquivoXML.Text;
    Result := True;
  finally
    ArquivoXML.Free;
  end;
end;

function TACBrXmlReader.CarregarArquivo(const Stream: TStringStream): boolean;
begin
  //NOTA: Carrega o arquivo xml na memória para posterior leitura de sua tag's
  FArquivo := Stream.DataString;
  Result := True;
end;

function TACBrXmlReader.ProcessarCNPJCPF(const ANode: TACBrXmlNode): string;
begin
  Result := ProcessarConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  if Trim(Result) = '' then
    Result := ProcessarConteudo(ANode.Childrens.Find('CPF'), tcStr);
end;

function TACBrXmlReader.ProcessarConteudo(const ANode: TACBrXmlNode; const Tipo: TpcnTipoCampo): variant;
var
  ConteudoTag: string;
begin
  if not Assigned(ANode) or (ANode = nil) then
    ConteudoTag := ''
  else
    ConteudoTag := Trim(ANode.Content);

  case Tipo of
    tcStr:
      result := ConteudoTag;

    tcDat:
      begin
        if length(ConteudoTag)>0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 01, 4)), StrToInt(copy(ConteudoTag, 06, 2)), StrToInt(copy(ConteudoTag, 09, 2)))
        else
          result := 0;
      end;

    tcDatVcto:
      begin
        if length(ConteudoTag)>0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 07, 4)), StrToInt(copy(ConteudoTag, 04, 2)), StrToInt(copy(ConteudoTag, 01, 2)))
        else
          Result := 0;
      end;

    tcDatCFe:
      begin
        if length(ConteudoTag)>0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 01, 4)), StrToInt(copy(ConteudoTag, 05, 2)), StrToInt(copy(ConteudoTag, 07, 2)))
        else
          result := 0;
      end;

    tcDatHor:
      begin
        if length(ConteudoTag)>0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 01, 4)), StrToInt(copy(ConteudoTag, 06, 2)), StrToInt(copy(ConteudoTag, 09, 2))) +
                    EncodeTime(StrToInt(copy(ConteudoTag, 12, 2)), StrToInt(copy(ConteudoTag, 15, 2)), StrToInt(copy(ConteudoTag, 18, 2)), 0)
        else
          result := 0;
      end;

    tcHor:
      begin
        if length(ConteudoTag)>0 then
          result := EncodeTime(StrToInt(copy(ConteudoTag, 1, 2)), StrToInt(copy(ConteudoTag, 4, 2)), StrToInt(copy(ConteudoTag, 7, 2)), 0)
        else
          result := 0;
      end;

    tcHorCFe:
      begin
        if length(ConteudoTag)>0 then
          result := EncodeTime(StrToInt(copy(ConteudoTag, 1, 2)), StrToInt(copy(ConteudoTag, 3, 2)), StrToInt(copy(ConteudoTag, 5, 2)), 0)
        else
          result := 0;
      end;

    tcDatHorCFe:
      begin
        if length(ConteudoTag)>0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 01, 4)), StrToInt(copy(ConteudoTag, 05, 2)), StrToInt(copy(ConteudoTag, 07, 2)))+
                    EncodeTime(StrToInt(copy(ConteudoTag, 09, 2)), StrToInt(copy(ConteudoTag, 11, 2)), StrToInt(copy(ConteudoTag, 13, 2)), 0)
        else
          result := 0;
      end;

    tcDe2, tcDe3, tcDe4, tcDe6, tcDe10:
      begin
        if length(ConteudoTag)>0 then
          result := StringToFloatDef(ConteudoTag, 0)
        else
          result := 0;
      end;

    tcEsp:
      result := ConteudoTag;

    tcInt:
      begin
        if length(ConteudoTag)>0 then
          result := StrToIntDef(Trim(OnlyNumber(ConteudoTag)),0)
        else
          result := 0;
      end;

  else
    raise Exception.Create('Node <' + ANode.Name + '> com conteúdo inválido. '+ ConteudoTag);
  end;
end;

end.

