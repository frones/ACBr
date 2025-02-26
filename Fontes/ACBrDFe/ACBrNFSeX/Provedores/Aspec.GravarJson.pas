{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Willian Delan de Oliveira                       }
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

unit Aspec.GravarJson;

interface

uses
  SysUtils, Classes, Variants, StrUtils,
  ACBrJSON,
  ACBrNFSeXGravarXml,
  ACBrNFSeXConversao;

type
  { TNFSeW_Aspec }

  TNFSeW_Aspec = class(TNFSeWClass)
  protected
    procedure Configuracao; override;

    function GerarPaisLocalPrestacaoServico: TACBrJSONObject;
    function GerarLocalPrestacaoServico: TACBrJSONObject;
    function GerarServicos: TACBrJSONObject;
    function GerarDadosNota: String;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o Json do RPS do provedor:
//     Aspec
//==============================================================================

{ TNFSeW_Aspec }

procedure TNFSeW_Aspec.Configuracao;
begin
  inherited Configuracao;

end;

function TNFSeW_Aspec.GerarXml: Boolean;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  FConteudoTxt.Clear;

  {$IFDEF FPC}
  FConteudoTxt.LineBreak := CRLF;
  {$ELSE}
    {$IFDEF DELPHI2006_UP}
    FConteudoTxt.LineBreak := CRLF;
    {$ENDIF}
  {$ENDIF}

  FConteudoTxt.Text := '[' + GerarDadosNota + ']';
  Result := True;
end;

function TNFSeW_Aspec.GerarDadosNota: String;
var
  AJSon: TACBrJSONObject;
begin
  AJSon := TACBrJsonObject.Create;
  try
    AJSon
       //Prestador
      .AddPair(IfThen(Length(OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.Cnpj)) = 11, 'cpfPessoaPrestador', 'cnpjPessoaPrestador'), NFSe.Prestador.IdentificacaoPrestador.Cnpj)
      //Tomador
//      .AddPair('tomador', Property)//Obtido em consulta prévia, caso o tomador já esteja cadastrado, exemplo abaixo.
//        "tomador": {
//         "id": number
//         },
      .AddPair(IfThen(Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) = 11, 'cpfPessoaTomador', 'cnpjPessoaTomador'), NFSe.Tomador.IdentificacaoTomador.CpfCnpj)
      .AddPair('nomePessoaTomador', Trim(NFSe.Tomador.RazaoSocial))
      .AddPair('razaoSocialPessoaTomador', Trim(NFSe.Tomador.RazaoSocial))
      .AddPair('nomeFantasiaTomador', Trim(NFSe.Tomador.NomeFantasia))
//      .AddPair('paisTomador', NFSe.Tomador.Endereco.CodigoPais)//Obrigatório informar se for do exterior, exemplo abaixo.
//        "paisTomador": {
//         "codigoBacen": number
//         },
      .AddPair('bairroId', NFSe.Tomador.Endereco.CodigoMunicipio)  //IDMunicipio ou IDBairro ou Código IBGE
      .AddPair('logradouroId', NFSe.Tomador.Endereco.CodigoMunicipio)     //IDMunicipio ou IDBairro ou Código IBGE
      .AddPair('bairroEnderecoTomador', Trim(NFSe.Tomador.Endereco.Bairro))
      .AddPair('logradouroEnderecoTomador', Trim(NFSe.Tomador.Endereco.Endereco))
      .AddPair('numeroEnderecoTomador', NFSe.Tomador.Endereco.Numero)
      .AddPair('complementoEnderecoTomador', Trim(NFSe.Tomador.Endereco.Complemento))
      .AddPair('inscricaoEstadualTomador', NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual)
      .AddPair('inscricaoMunicipalTomador', NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal)
      .AddPair('dddFixoTomador', Copy(NFSe.Tomador.Contato.Telefone, 1, 3))
      .AddPair('telefoneFixoTomador', Copy(NFSe.Tomador.Contato.Telefone,3,11))
      .AddPair('emailTomador', Trim(NFSe.Tomador.Contato.Email))
      .AddPair('paisLocalPrestacaoServico', GerarPaisLocalPrestacaoServico)  //Pode usar o id ou o código Bacen
      .AddPair('localPrestacaoServico', GerarLocalPrestacaoServico) //Pode usar o id ou o código IBGE  - Lista
      .AddPair('servico', GerarServicos) //Pode usar o id ou o código do serviço
      .AddPair('aliquota', NFSe.Servico.Valores.Aliquota)
      .AddPair('issRetidoPeloTomador', IfThen(NFSe.Servico.Valores.IssRetido = stRetencao, 'SIM', 'NAO'))
      .AddPair('discriminacaoServico', NFSe.Servico.Discriminacao)
      .AddPair('valorTotal', NFSe.Servico.Valores.ValorServicos)
      .AddPair('valorDeducoes', NFSe.Servico.Valores.ValorDeducoes)
      .AddPair('descontoCondicionado', NFSe.Servico.Valores.DescontoCondicionado)
      .AddPair('descontoIncondicionado', NFSe.Servico.Valores.DescontoIncondicionado)
      .AddPair('valorBaseCalculo', NFSe.Servico.Valores.BaseCalculo)
      .AddPair('cofins', NFSe.Servico.Valores.ValorCofins)
      .AddPair('csll', NFSe.Servico.Valores.ValorCsll)
      .AddPair('inss', NFSe.Servico.Valores.ValorInss)
      .AddPair('irrf', NFSe.Servico.Valores.ValorIr)
      .AddPair('pisPasep', NFSe.Servico.Valores.ValorPis)
      .AddPair('rpsDataEmissaoStr', FormatDateTime('dd/mm/yyyy', NFSe.DataEmissaoRps))
      .AddPair('rpsSerie', NFSe.IdentificacaoRps.Serie)
      .AddPair('rpsNumero', StrToInt(NFSe.IdentificacaoRps.Numero))
      .AddPair('tokenRPS', ChaveAcesso);
    Result := AJSon.ToJSON;
  finally
    AJSon.Free;
  end;
end;

function TNFSeW_Aspec.GerarPaisLocalPrestacaoServico: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('codigoBacen', NFSe.Tomador.Endereco.CodigoPais);
end;

function TNFSeW_Aspec.GerarLocalPrestacaoServico: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('codIBGE', StrToInt(NFSe.Tomador.Endereco.CodigoMunicipio));
end;

function TNFSeW_Aspec.GerarServicos: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create
              .AddPair('codigo', NFSe.Servico.ItemListaServico);
end;

end.
