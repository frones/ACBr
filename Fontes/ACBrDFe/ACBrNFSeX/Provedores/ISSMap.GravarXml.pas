{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ISSMap.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml;

type
  { Provedor com layout próprio }
  { TNFSeW_ISSMap }

  TNFSeW_ISSMap = class(TNFSeWClass)
  protected

  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  synacrypt,
  synacode,
  DateUtils,
  Variants,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     ISSMap
//==============================================================================

{ TNFSeW_ISSMap }

function TNFSeW_ISSMap.GerarXml: Boolean;
var
  NFSeNode: TACBrXmlNode;
  aes: TSynaAes;

function cryptAES(aDado: string): string;
begin
  if aDado <> '' then
    Result := EncodeBase64(aes.EncryptECB(aDado))
  else
    Result := '';
end;

function cryptAESDe2(aDado: Double; Obrigatorio: Boolean = False): string;
begin
  if (aDado > 0) or Obrigatorio then
    Result := EncodeBase64(aes.EncryptECB(FormatFloat('#.00', aDado)))
  else
    Result := '';
end;

function cryptAESDat(aDado: TDateTime): string;
var
  wAno, wMes, wDia, wHor, wMin, wSeg, wMse: word;
  xDado: string;
begin
  DecodeDateTime(VarToDateTime(aDado), wAno, wMes, wDia, wHor, wMin, wSeg, wMse);
  xDado := FormatFloat('0000', wAno) + '-' +
    FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia) +
    'T' + FormatFloat('00', wHor) + ':' + FormatFloat('00', wMin) +
    ':' + FormatFloat('00', wSeg);

  Result := EncodeBase64(aes.EncryptECB(xDado));
end;

begin
  // ChaveAutoriz - Chave de criptografia fornecida pelo sistema ISSMap
  aes := TSynaAes.Create(ChaveAutoriz);

  try
    Configuracao;

    ListaDeAlertas.Clear;

    FDocument.Clear();

    NFSeNode := CreateElement('rps');

    FDocument.Root := NFSeNode;

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'aliquota', 1, 5, 1,
                               cryptAESDe2(NFSe.Servico.Valores.Aliquota), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'status', 1, 10, 1,
                                                       cryptAES('normal'), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cepPrestador', 1, 8, 1,
                                    cryptAES(NFSe.Prestador.Endereco.CEP), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cepTomador', 1, 8, 1,
                                      cryptAES(NFSe.Tomador.Endereco.CEP), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cidadePrestador', 1, 100, 1,
                             cryptAES(NFSe.Prestador.Endereco.xMunicipio), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cidadeTomador', 1, 100, 1,
                               cryptAES(NFSe.Tomador.Endereco.xMunicipio), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'codigoServicoPrestado', 1, 5, 1,
                                  cryptAES(NFSe.Servico.ItemListaServico), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cpfCnpjPrestador', 1, 14, 1,
                  cryptAES(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cpfCnpjTomador', 1, 14, 1,
                      cryptAES(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'numeroNfse', 1, 15, 1,
                                                    cryptAES(NFSe.Numero), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'dataHoraEmissao', 1, 19, 1,
                                         cryptAESDat(NFSe.DataEmissaoRps), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'descServico', 1, 4000, 1,
                                     cryptAES(NFSe.Servico.Discriminacao), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'emailPrestador', 1, 60, 0,
                                   cryptAES(NFSe.Prestador.Contato.Email), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'emailTomador', 1, 60, 0,
                                     cryptAES(NFSe.Tomador.Contato.Email), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'enderecoPrestador', 1, 120, 1,
                               cryptAES(NFSe.Prestador.Endereco.Endereco), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'enderecoTomador', 1, 120, 1,
                                 cryptAES(NFSe.Tomador.Endereco.Endereco), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'estadoPrestador', 1, 2, 1,
                                     cryptAES(NFSe.Prestador.Endereco.UF), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'estadoTomador', 1, 2, 1,
                                       cryptAES(NFSe.Tomador.Endereco.UF), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ieRgPrestador', 1, 30, 0,
        cryptAES(NFSe.Prestador.IdentificacaoPrestador.InscricaoEstadual), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ieRgTomador', 1, 30, 0,
            cryptAES(NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'imPrestador', 1, 30, 0,
       cryptAES(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'imTomador', 1, 30, 0,
           cryptAES(NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal), ''));

    // Não deve ser criptografado
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Key', 1, 19, 1, ChaveAcesso, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'localExecucao', 1, 32, 1,
                         cryptAES(NFSe.Servico.MunicipioPrestacaoServico), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'motivoCancelamento', 1, 500, 0,
                                        cryptAES(NFSe.MotivoCancelamento), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'nomeRazaoPrestador', 1, 120, 1,
                                     cryptAES(NFSe.Prestador.RazaoSocial), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'nomeRazaoTomador', 1, 120, 1,
                                       cryptAES(NFSe.Tomador.RazaoSocial), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'numeroRPS', 1, 19, 1,
                                   cryptAES(NFSe.IdentificacaoRps.Numero), ''));

    if NFSe.Servico.Valores.IssRetido = stRetencao then
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'retidoNaFonte', 1, 1, 1,
                                                             cryptAES('S'), ''))
    else
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'retidoNaFonte', 1, 1, 1,
                                                            cryptAES('N'), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'servicoPrestado', 1, 1000, 0,
                                 cryptAES(NFSe.Servico.xItemListaServico), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorBaseCalculo', 1, 15, 1,
                            cryptAESDe2(NFSe.Servico.Valores.BaseCalculo), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorDeducoes', 1, 15, 1,
                    cryptAESDe2(NFSe.Servico.Valores.ValorDeducoes, True), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorIss', 1, 15, 1,
                               cryptAESDe2(NFSe.Servico.Valores.ValorIss), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorNota', 1, 15, 1,
                       cryptAESDe2(NFSe.Servico.Valores.ValorLiquidoNfse), ''));

    // codigoVerifica - Opcional
    // tipoTributacao - Opcional

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'naturezaOperacao', 1, 100, 1,
                   cryptAES(NaturezaOperacaoToStr(NFSe.NaturezaOperacao)), ''));

    // tipoMovimento - Opcional

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorCredito', 1, 15, 1,
                                     cryptAESDe2(NFSe.ValorCredito, True), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'numeroNFSE', 1, 19, 1,
                                                    cryptAES(NFSe.Numero), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'dataHoraEmissaoRPS', 1, 19, 1,
                                         cryptAESDat(NFSe.DataEmissaoRps), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'resposta', 1, 3, 1,
                                                cryptAES(NFSe.NumeroLote), ''));



    (*
<codigoVerifica>u79BxiYQYnPCtMysOB4lNwjvV0JvFlrgrxwbkLrXlSs=</codigoVerifica>
<numero>hlqPEjVOvSEokzLEU+QqDQ==</numero>
<pass>H2aamvBtbCkzvpDDLfqA+g==</pass>
<porcentagemCOFINS>8RdZqq07PewGuVD1OqX57NrZfR+0PKDOzDSZReQEYbI=</porcentagemCOFINS>
<porcentagemCSLL>hQhFMqGNQtC2QgYY8Gv94Oc9Nfzz5wseMmHNstS1kzI=</porcentagemCSLL>
<porcentagemINSS>XUe2RVAZOo5X+NwKuTVJn/EkIr7FJXrrppHXwtTaROY=</porcentagemINSS>
<porcentagemIRRF>Q7xwjLIdU9DRKOTH+PpAY4fbaa86KUtu23ICjcoAc8c=</porcentagemIRRF>
<porcentagemOutros>z9PEPr3tlYX2JpC146ETSjBOs2cHrRmq/XGoWlt4K8g=</porcentagemOutros>
<porcentagemPIS>V8V3Ck1T+ixSgQBhVuM7ac7vKw7ZzZOh1bpDF3xRu/s=</porcentagemPIS>
<retencaoObrigatoriaTomador>In7MRJupfqlh4SrfxjrMUzQUHOyWvTuNUbDJITDNMyYCAd3wSqJ3WK8T/KbLed/j</retencaoObrigatoriaTomador>
<status>5RvbyLpsQTHfw6pqM0GoCw==</status>
<valorCOFINS>4cfmYBqp9PcxksaSf9vZ24kBU0D68dkhmBJ6YhtCQwk=</valorCOFINS>
<valorCSLL>cZC3YcqGJiCo0izfH2Tzyw==</valorCSLL>
<valorINSS>aXdOGktSU6BayPr8uIkWsw==</valorINSS>
<valorIRRF>9sAicID1YOm29LWE6CK9bg==</valorIRRF>
<valorOutros>Mg7y5/6wFWgtMuxVitpo7okBU0D68dkhmBJ6YhtCQwk=</valorOutros>
<valorPIS>ULtUsqpaF+Fl0o2gyBOxRQ==</valorPIS>


    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Pass', 1, 100, 1,
                                                          cryptAES(Senha), ''));

//    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cancelada', 1, 10, 0,
//                                                       cryptAES('normal'), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'porcentagemPIS', 1, 15, 0,
                            cryptAESDe2(NFSe.Servico.Valores.AliquotaPis), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'porcentagemCOFINS', 1, 15, 0,
                         cryptAESDe2(NFSe.Servico.Valores.AliquotaCofins), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'porcentagemCSLL', 1, 15, 0,
                           cryptAESDe2(NFSe.Servico.Valores.AliquotaCsll), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'porcentagemIRRF', 1, 15, 0,
                             cryptAESDe2(NFSe.Servico.Valores.AliquotaIr), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'porcentagemINSS', 1, 15, 0,
                           cryptAESDe2(NFSe.Servico.Valores.AliquotaInss), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'porcentagemOutros', 1, 15, 0,
                       cryptAESDe2(NFSe.Servico.Valores.AliquotaDeducoes), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'porcentagemOutros', 1, 15, 0,
                       cryptAESDe2(NFSe.Servico.Valores.AliquotaDeducoes), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorPIS', 1, 15, 0,
                               cryptAESDe2(NFSe.Servico.Valores.ValorPis), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorCOFINS', 1, 15, 0,
                            cryptAESDe2(NFSe.Servico.Valores.ValorCofins), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorCSLL', 1, 15, 0,
                              cryptAESDe2(NFSe.Servico.Valores.ValorCsll), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorCSLL', 1, 15, 0,
                              cryptAESDe2(NFSe.Servico.Valores.ValorCsll), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorIRRF', 1, 15, 0,
                              cryptAESDe2(NFSe.Servico.Valores.ValorIr), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorINSS', 1, 15, 0,
                              cryptAESDe2(NFSe.Servico.Valores.ValorInss), ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'valorOutros', 1, 15, 0,
                   cryptAESDe2(NFSe.Servico.Valores.valorOutrasRetencoes), ''));

    // retencaoObrigatoriaTomador (opcional) true ou false
    *)
    Result := True;
  finally
    aes.free;
  end;
end;

end.
