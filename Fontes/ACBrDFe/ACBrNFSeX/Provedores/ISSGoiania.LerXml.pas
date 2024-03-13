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

unit ISSGoiania.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrNFSeXLerXml_ABRASFv2, ACBrXmlDocument, ACBrXmlBase,
  ACBrUtil.FilesIO, ACBrUtil.Strings;

const
  ARQUIVO_CIDADES_ISSGOIANIA = 'CidadesISSGoiania.txt';

type
  { TNFSeR_ISSGoiania200 }

  TNFSeR_ISSGoiania200 = class(TNFSeR_ABRASFv2)
  private
    function ObterNomeMunicipioISSGoiania(acMun: String; var xUF: String): String;
  protected
    procedure LerEnderecoPrestadorServico(const ANode: TACBrXmlNode; const aTag: string); override;
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode); override;
  public

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     ISSGoiania
//==============================================================================

{ TNFSeR_ISSGoiania200 }

procedure TNFSeR_ISSGoiania200.LerEnderecoPrestadorServico(
  const ANode: TACBrXmlNode; const aTag: string);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs(aTag);

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Endereco'), tcStr);
      Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento     := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
      CodigoPais      := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoPais'), tcInt);
      CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);

      xMunicipio      := ObterNomeMunicipioISSGoiania(CodigoMunicipio, xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_ISSGoiania200.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF, xEndereco: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Endereco'), tcStr);
      Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento     := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
      CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
      xMunicipio      := ObterNomeMunicipioISSGoiania(CodigoMunicipio, xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;

  AuxNode := ANode.Childrens.FindAnyNs('EnderecoExterior');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      xEndereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('EnderecoCompletoExterior'), tcStr);

      if xEndereco <> '' then
        Endereco := xEndereco;

      CodigoPais := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoPais'), tcInt);

      if (CodigoPais <> 1058) and (CodigoPais > 0) then
        UF := 'EX';
    end;
  end;
end;

function TNFSeR_ISSGoiania200.ObterNomeMunicipioISSGoiania(acMun: String; var xUF: String): String;
var
  listaCidades: TStringList;
  caminhoArquivo, codMunicipioNaLista, linhaArquivo: String;
  i, primeiroPontoEVirgula: integer;
begin
  Result := '';

  caminhoArquivo := ApplicationPath + ARQUIVO_CIDADES_ISSGOIANIA;

  if FileExists(caminhoArquivo) then
  begin
    listaCidades := TStringList.Create;
    try
      listaCidades.LoadFromFile(caminhoArquivo);

      if listaCidades.Count <= 0 then
        exit;

      acMun := Copy(acMun, 2, Length(acMun));

      for i:=0 to listaCidades.Count - 1 do
      begin
        linhaArquivo := listaCidades.Strings[i];
        primeiroPontoEVirgula := PosAt(';', linhaArquivo);
        codMunicipioNaLista := Copy(linhaArquivo, 0, primeiroPontoEVirgula - 1);

        if codMunicipioNaLista = acMun then
        begin
          xUF := Trim(Copy(linhaArquivo, PosAt(';', linhaArquivo, 2) + 1, Length(linhaArquivo)));
          Result := Trim(RetornarConteudoEntre(linhaArquivo, ';', ';'));
          break;
        end;
      end;

    finally
      listaCidades.Free;
    end;
  end;
end;

end.
