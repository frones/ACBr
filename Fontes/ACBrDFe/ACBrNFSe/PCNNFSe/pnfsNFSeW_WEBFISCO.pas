{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
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
{ Fernando Castelano Banhos -  fernado@lucedata.com.br  -  www.lucedata.com.br }
{ Alameda Demetrio Cavlak, 1377 - Lucélia - SP - 17780-000                     }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pnfsNFSeW_WEBFISCO;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  synacode, ACBrConsts,
  pnfsNFSeW,
  pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsConversao, pnfsConsts;

type
  { TNFSeW_WEBFISCOPublico }

  TNFSeW_WEBFISCO = class(TNFSeWClass)
  protected
    procedure GerarNotas;
  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil, MaskUtils;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do provedor WEBFISCO.                                          }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_WEBFISCO }

constructor TNFSeW_WEBFISCO.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

procedure TNFSeW_WEBFISCO.GerarNotas;
begin
  Gerador.wGrupo('definitions xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ' +
                 'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                 'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                 'xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" ' +
                 'xmlns:tns="https://www.webfiscotecnologia.com.br/issqn/ws/wsnfe_teste_homologacao.php" ' +
                 'xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" ' +
                 'xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" ' +
                 'xmlns="http://schemas.xmlsoap.org/wsdl/" ' +
                 'targetNamespace="https://www.webfiscotecnologia.com.br/issqn/ws/wsnfe_teste_homologacao.php"');

  Gerador.wGrupo('types');
  Gerador.wGrupo('xsd:schema targetNamespace="https://www.webfiscotecnologia.com.br/issqn/ws/wsnfe_teste_homologacao.php"');
  Gerador.wCampoNFSe(tcStr, '', 'xsd:import namespace="http://schemas.xmlsoap.org/soap/encoding/"', 1, 1, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'xsd:import namespace="http://schemas.xmlsoap.org/wsdl/"', 1, 1, 1, '', '');
  Gerador.wGrupo('xsd:complexType name="StrEnvNfe"');
  Gerador.wGrupo('xsd:complexContent');
  Gerador.wGrupo('xsd:restriction base="SOAP-ENC:Array"');
  Gerador.wCampoNFSe(tcStr, '', 'xsd:attribute ref="SOAP-ENC:arrayType" wsdl:arrayType="tns:EnvNfe[]"', 1, 1, 1, '', '');
  Gerador.wGrupo('/xsd:restriction');
  Gerador.wGrupo('/xsd:complexContent');
  Gerador.wGrupo('/xsd:complexType');
  Gerador.wGrupo('xsd:complexType name="EnvNfe"');
  Gerador.wGrupo('xsd:all');
  Gerador.wCampoNFSe(tcStr, '', 'xsd:element name="okk" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wGrupo('/xsd:all');
  Gerador.wGrupo('/xsd:complexType');
  Gerador.wGrupo('/xsd:schema');
  Gerador.wGrupo('/types');

  Gerador.wGrupo('message name="EnvNfeRequest"');
  Gerador.wCampoNFSe(tcStr, '', 'part name="usuario" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wGrupo('usuario xsi:type="xsd:string"');
  Gerador.wTexto('901567'); //revisar
  Gerador.wGrupo('/usuario');
  Gerador.wCampoNFSe(tcStr, '', 'part name="pass" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wGrupo('pass xsi:type="xsd:string"');
  Gerador.wTexto('123456'); //revisar
  Gerador.wGrupo('/pass');
  Gerador.wCampoNFSe(tcStr, '', 'part name="prf" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' );  //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="usr" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Prestador.Cnpj);
  Gerador.wCampoNFSe(tcStr, '', 'part name="ctr" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->00001');  //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="cnpj" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.IdentificacaoTomador.CpfCnpj);
  Gerador.wCampoNFSe(tcStr, '', 'part name="cnpjn" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.RazaoSocial);
  Gerador.wCampoNFSe(tcStr, '', 'part name="ie" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual);
  Gerador.wCampoNFSe(tcStr, '', 'part name="im" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal);
  Gerador.wCampoNFSe(tcStr, '', 'part name="lgr" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.Endereco.Endereco);
  Gerador.wCampoNFSe(tcStr, '', 'part name="num" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.Endereco.Numero);
  Gerador.wCampoNFSe(tcStr, '', 'part name="cpl" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.Endereco.Complemento);
  Gerador.wCampoNFSe(tcStr, '', 'part name="bai" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.Endereco.Bairro);
  Gerador.wCampoNFSe(tcStr, '', 'part name="cid" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.Endereco.xMunicipio);
  Gerador.wCampoNFSe(tcStr, '', 'part name="est" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.Endereco.UF);
  Gerador.wCampoNFSe(tcStr, '', 'part name="cep" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.Endereco.CEP);
  Gerador.wCampoNFSe(tcStr, '', 'part name="fon" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.Contato.Telefone);
  Gerador.wCampoNFSe(tcStr, '', 'part name="mail" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Tomador.Contato.Email);
  Gerador.wCampoNFSe(tcStr, '', 'part name="item1" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->99.99'); //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="aliq1" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->2.00'); //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="val1" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + CurrToStr(NFSe.Servico.ItemServico.Items[0].ValorUnitario)); //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="loc" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.PrestadorServico.Endereco.CodigoMunicipio); //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="ret" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->NAO'); //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="txt" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + NFSe.Servico.ItemServico.Items[0].Descricao); //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="val" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + CurrToStr(NFSe.Servico.Valores.ValorServicos)); //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="valtrib" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + CurrToStr(NFSe.Servico.Valores.ValorServicos)); //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="iss" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + CurrToStr(NFSe.Servico.Valores.ValorIss)); //revisar
  Gerador.wCampoNFSe(tcStr, '', 'part name="issret" type="xsd:string"', 1, 1, 1, '', '');
  Gerador.wTexto('->' + CurrToStr(NFSe.Servico.Valores.ValorIssRetido)); //revisar
  Gerador.wGrupo('/message');

  Gerador.wGrupo('message name="EnvNfeResponse"');
  Gerador.wCampoNFSe(tcStr, '', 'part name="return" type="tns:StrEnvNfe"', 1, 1, 1, '', '');
  Gerador.wGrupo('/message');
  Gerador.wGrupo('portType name="webservicePortType"');
  Gerador.wGrupo('operation name="EnvNfe"');
  Gerador.wCampoNFSe(tcStr, '', 'input message="tns:EnvNfeRequest"', 1, 1, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'output message="tns:EnvNfeResponse"', 1, 1, 1, '', '');
  Gerador.wGrupo('/operation');
  Gerador.wGrupo('/portType');
  Gerador.wGrupo('binding name="webserviceBinding" type="tns:webservicePortType"');
  Gerador.wCampoNFSe(tcStr, '', 'soap:binding style="rpc" transport="http://schemas.xmlsoap.org/soap/http"', 1, 1, 1, '', '');
  Gerador.wGrupo('operation name="EnvNfe"');
  Gerador.wCampoNFSe(tcStr, '', 'soap:operation soapAction="https://www.webfiscotecnologia.com.br/issqn/wservice/wsnfe_teste_homologacao.php/EnvNfe" style="rpc"', 1, 1, 1, '', '');
  Gerador.wGrupo('input');
  Gerador.wCampoNFSe(tcStr, '', 'soap:body use="encoded" namespace="" encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"', 1, 1, 1, '', '');
  Gerador.wGrupo('/input');
  Gerador.wGrupo('output');
  Gerador.wCampoNFSe(tcStr, '', 'soap:body use="encoded" namespace="" encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"', 1, 1, 1, '', '');
  Gerador.wGrupo('/output');
  Gerador.wGrupo('/operation');
  Gerador.wGrupo('/binding');
  Gerador.wGrupo('service name="webservice"');
  Gerador.wGrupo('port name="webservicePort" binding="tns:webserviceBinding"');
  Gerador.wCampoNFSe(tcStr, '', 'soap:address location="https://www.webfiscotecnologia.com.br/issqn/wservice/wsnfe_teste_homologacao.php"', 1, 1, 1, '', '');
  Gerador.wGrupo('/port');
  Gerador.wGrupo('/service');
  Gerador.wGrupo('/definitions');
end;

function TNFSeW_WEBFISCO.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  FDefTipos := FServicoEnviar;

  GerarNotas;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

function TNFSeW_WEBFISCO.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

end.
