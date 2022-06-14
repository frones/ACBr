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
unit ISSBarueri.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type

  { TACBrNFSeXWebserviceISSBarueri }

  TACBrNFSeXWebserviceISSBarueri = class(TACBrNFSeXWebserviceSoap12)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
  end;

  { TACBrNFSeProviderISSBarueri }

  TACBrNFSeProviderISSBarueri = class(TACBrNFSeProviderProprio)
  private
    function ExisteErroRegistro(const ALinha: String): Boolean;
  protected
    procedure Configuracao; override;
    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;
    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse); override;
    function PrepararRpsParaLote(const aXml: string): string; override;
    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    function AplicarXMLtoUTF8(AXMLRps: String): String; override;
    function AplicarLineBreak(AXMLRps: String; const ABreak: String): String; override;
    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'ListaMensagemRetorno';
                                     const AMessageTag: string = 'MensagemRetorno'); override;
  end;

  { TACBrNFSeProviderBarueriErros }

  TACBrNFSeProviderBarueriErros = class(TStringList)
  public
    constructor Create;
    function Causa(const ACodigo: String): String;
    function Solucao(const ACodigo: String): String;
  end;

implementation

uses
  synacode, synautil,
  pcnAuxiliar, ACBrConsts,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ISSBarueri.GravarXml, ISSBarueri.LerXml;

{ TACBrNFSeProviderBarueriErros }

constructor TACBrNFSeProviderBarueriErros.Create;
begin
  Add('100=Tipo de Registro Inválido|Informar Tipo Especificado: 1');
  Add('101=Inscrição do Prestador de Serviços não encontrada na base de dados da PMB|Informar Número Correto do Prestador de Serviços');
  Add('102=Identificação da Remessa do Contribuinte inválida ou já informada em outro arquivo de remessa|Informar Número válido e único/exclusivo. Um número outra enviado jamais poderá ser enviado novamente');
  Add('200=Tipo de Registro Inválido|Informar Tipo Especificado: 2');
  Add('201=Tipo de RPS Inválido|Informar Tipo Especificado: RPS / RPS-C');
  Add('202=Número de Série do RPS Inválida|Informar o Número de Série do RPS Válida');
  Add('203=Número de Série da Nf-e Inválida|Informar o Número de Série da NF-e Válida');
  Add('204=Número de RPS não Informado ou inválido. Numeração máxima permitida 0009999999|Informar o Número do RPS');
  Add('205=Número de RPS já enviado|Informar um Número de RPS Válido');
  Add('206=Numero do RPS enviado em Duplicidade no Arquivo|Informar o RPS apenas uma vez no arquivo, caso envie vários arquivos simultâneos enviar o RPS uma vez em apenas 1 dos arquivos.');
  Add('207=NF-e não consta na base de dados da PMB, não pode ser cancelada/substituida|Informar NF-e Válida.');
  Add('208=Data Inválida|A data informada deverá estar no formato AAAAMMDD, ou seja, 4 dígitos para ano seguido de 2 dígitos para o mês seguido de 2 dígitos para o dia.');
  Add('209=Data de Emissão não poderá ser inferior a 09/09/2008|Informar uma Data Válida');
  Add('210=Data de Emissão do RPS não pode ser superior a Data de Hoje|Informar uma Data Válida');
  Add('211=Hora de Emissão do RPS Inválida|A hora informada deverá estar no formato HHMMSS, ou seja, 2 dígitos para hora em seguida 2 dígitos para os minutos e e 2 dígitos para os segundos.');
  Add('212=Situação do RPS Inválida|Informar a Situação Especificada: E para RPS Enviado / C para RPS Cancelado');
  Add('213=Código do Motivo de Cancelamento Inválido|Informar o Código Especificado: 01 para Extravio / 02 para Dados Incorretos / 03 para Substituição');
  Add('214=Campo Descrição do Cancelamento não informado|Informar a Descrição do Cancelamento');
  Add('215=NFe não pode ser cancelada, guia em aberto para nota fiscal correspondente|Cancelar a guia correspondente a nota fiscal');
  Add('216=Código de Atividade não encontrada na base da PMB|Informar Código de Atividade Válido');
  Add('217=Local da Prestação do Serviço Inválido|Informar o Local Especificado:1 para serviço prestado no Município / 2 para serviço prestado fora do Município / 3 para serviço prestado fora do País');
  Add('218=Serviço Prestado em Vias Públicas Inválido|Informe 1 para serviço prestado em vias públicas / 2 para serviço não prestado em vias públicas.');
  Add('219=Campo Endereco do Serviço Prestado é obrigatório|Informar Endereço');
  Add('220=Campo Número do Logradouro do Serviço Prestado é obrigatório|Informar Número');
  Add('221=Campo Bairro do Serviço Prestado é obrigatório|Informar Bairro');
  Add('222=Campo Cidade do Serviço Prestado é obrigatório|Informar Cidade');
  Add('223=Campo UF do Serviço Prestado é obrigatório|Informar UF Tomador');
  Add('224=Campo UF do Serviço Prestado invalido|Informar UF Tomador Válida');
  Add('225=Campo CEP do Serviço Prestado invalido|Informar CEP');
  Add('226=Quantidade de Serviço não deverá ser inferior a zero e/ou Quantidade de Serviço deverá ser numérico|Informar um Valor Válido.');
  Add('227=Valor do Serviço não pode ser menor que zero e/ou Valor do Serviço deverá ser numérico|Informar um Valor Válido');
  Add('228=Reservado');
  Add('229=Reservado');
  Add('230=Valor Total das Retenções não deverá ser inferior a zero e/ou Valor Total das Retenções deverá ser numérico|Informar um Valor Válido.');
  Add('231=Valor Total das Retenções não poderá ser superior ao Valor Total do serviço prestado|Informar um Valor Válido.');
  Add('232=Valor Total dos Retenções não confere com os valores deduçoes informadas para este RPS|Informar Somatória dos Valores de Retenções informadas no registro 3 referente a este RPS');
  Add('233=Identificador de tomodor estrangeiro inválido|Informe 1 Para Tomador Estrangeiro 2 para Tomador Brasileiro');
  Add('234=Código do Pais de Nacionalidade do Tomador Estrangeiro inválido|Informe um código de pais válido');
  Add('235=Identificador se Serviço Prestado é exportação inválido|Informe 1 Para Serviço exportado 2 para Serviço não exportado.');
  Add('236=Indicador CPF/CNPJ Inválido|Informar Indicador do CPF/CNPJ Especificado:1 para CPF / 2 para CNPJ');
  Add('237=CPNJ do Tomador Inválido|Informar o CPNJ do Tomador Válido');
  Add('238=Campo Nome ou Razão Social do Tomador de Serviços é Obrigatório|Informar Razão Social');
  Add('239=Campo Endereço do Tomador de Serviços é Obrigatório|Informar Endereço');
  Add('240=Campo Número do Logradouro do Tomador de Serviços|Informar Número');
  Add('241=Campo Bairro do Tomador de Serviços é Obrigatório|Informar Bairro');
  Add('242=Campo Cidade do Tomador de Serviços é Obrigatório|Informar Cidade');
  Add('243=Campo UF do Tomador de Serviços é Obrigatório|Informar UF Tomador');
  Add('244=Campo UF do Tomador de Serviços Inválido|Informar UF Tomador Válida');
  Add('245=Campo CEP do Tomador de Serviços Inválido|Informar CEP');
  Add('246=Email do Tomador de Serviços Inválido|Informar e-mail Válido');
  Add('247=Campo Fatura deverá ser numérico|Informar um conteudo válido.');
  Add('248=Valor da Fatura não deverá ser inferior a zero e/ou Valor da Fatura deverá ser numérico|Informar um Valor Válido');
  Add('249=Campo Forma de Pagamento não informado|Informar Forma de Pagamento');
  Add('250=Campo Discriminação de Serviço não informado e/ou fora dos padrões estabelecidos no layout|Informar a Discriminação do Serviço');
  Add('251=Valor Total do Serviço superior ao permitido (campo valor do serviço multiplicado pelo campo quantidade)|Informar valores validos');
  Add('252=Data Inválida|A data informada deverá estar no formato AAAAMMDD, ou seja, 4 dígitos para ano seguido de 2 dígitos para o mês seguido de 2 dígitos para o dia.');
  Add('253=NFe não pode ser cancelada, data de emissão superior a 90 dias|Informar NF-e valida para cancelamento/substituição');
  Add('254=Nota Fiscal Já Cancelada|Informar NF-e valida para cancelamento/substituição');
  Add('255=Nota Fiscal com valores zerados|O valor da nota fiscal é calculado: (quantidade do serviço x preço unitário) + valor "VN" informado no registro 3. Esse resultado pode ser zero desde que o valor do serviço ou VN seja diferente de zero.');
  Add('256=Contribuinte com condição diferente de ativo|Artigo 3º. Os contribuintes com restrições cadastrais estão impedidos de utilizar os sistemas ora instituídos. ' +
      '-Contribuinte com situação diferente de ativo não poderá converter RPS emitidos após a data da alteração da situação. Dúvidas entrar em contato com o Depto. Técnico de Tributos Mobiliários no Tel. 4199-8050.');
  Add('257=Nota Fiscal enviada em Duplicidade no Arquivo|Informar a Nota Fiscal apenas uma vez no arquivo, caso envie vários arquivos simultâneos enviar a Nota uma vez em apenas 1 dos arquivos.');
  Add('258=NFe não pode ser cancelada ou substituida competência já encerrada|Para prosseguir é necessário retificar o movimento através do menu "Retificar Serviços Prestados"');
  Add('259=Data de Emissão do RPS refere-se a competência já encerrada|Para prosseguir é necessário retificar o movimento através do menu "Retificar Serviços Prestados"');
  Add('260=Código de Atividade não permitido|Informar um Código de Atividade vinculado ao Perfil do Contribuinte ou um Código de Atividade tributada.');
  Add('261=Código de Atividade Bloqueado|Informar um Código de Atividade vinculado ao Perfil do Contribuinte (Atingido o limite permitido de notas para atividade não vinculadas ao cadastro do contribuinte).');
  Add('300=Tipo de Registro Inválido|Informar Tipo Especificado: 3');
  Add('301=Código de Outros Valores Inválido|Informar o Código Especificado: 01 - para IRRF 02 - para PIS/PASEP 03 - para COFINS 04 - ' +
      'para CSLL VN - para Valor não Incluso na Base de Cálculo (exceto tributos federais). Esse campo somado ao valor total dos serviços resulta no Valor total da nota.');
  Add('302=Caso seja retenção: Valor da Retenção não poderá ser menor/igual a zero Caso seja "VN" Valor deve ser diferente de zero|Caso ' +
      'não tenha retenção não informar o registro ou informe um valor maior que zero. Se for "VN" informar um valor diferente de zero ou simplesmente não informe esse registro');
  Add('303=Soma do serviço prestado e valor "VN" não poderá ser inferior a zero.|Informar um Valor Válido.');
  Add('304=Código de Outros Valores envia|Informar Código de Retenção Válido');
  Add('305=Conforme Lei Complementar 419 / 2017, ficam revogados, a partir de 30 de dezembro de 2017, todos os regimes especiais e soluções ' +
      'de consulta cujo resultado ermitiu redução do preço do serviço ou da base de cálculo do Imposto Sobre Serviço de Qualquer Natureza.|Não informar este tipo de dedução para RPS cuja a data base de cálculo seja superior à 29/12/2017.');
  Add('400=Tipo de Registro Inválido|Informar Tipo Especificado: 9');
  Add('401=Número de Linhas não confere com número de linhas do tipo 1,2,3 e 9 enviadas no arquivo|Informe o número de linhas (tipo 1,2,3 e 9)');
  Add('402=Valor Total dos Serviços não confere os valores de serviços enviados no arquivo|Informar Somatória dos Valores Totais de Serviços Prestados (Soma dos valores dos serviços multiplicados pelas quantidades de cada serviço)');
  Add('403=Valor Total das Retenções e Total de outros valores informados no registro 3 não confere com total informado|Informar Somatória dos Valores Totais lançados no Registro tipo 3.');
  Add('000=Lay-Out do arquivo fora dos padrões|O arquivo enviado não é um arquivo de Remessa da NFe de Barueri. Enviar um arquivo com os Registros: 1, 2, 9 e opcionalmente o registro tipo 3');
  Add('500=Lay-Out do arquivo fora dos padrões|Os registros válidos esperados no arquivo são tipo: 1,2,3 e 9');
  Add('600=Lay-Out do arquivo fora dos padrões|Deve haver apenas 1 registro tipo 9 e esse deve ser o último registro do arquivo');
  Add('700=Quantidade de RPS superior ao padrão|Enviar um arquivo com no máximo 1000 RPS');
  Add('900=Tamanho do Registro diferente da especificação do layout|Reveja as posições/tamanho para cada registro, certifique-se que o tamanho dos registros conferem com o layout e contém o caracter de fim de linha conforme especificado no layout');
  Add('901=Arquivo com ausência de um dos Registros: 1, 2 ou 9|Reveja os registros do arquivo, certifique-se que todos registros mencionados estão presentes em seu arquivo. ' +
      'Também certifique-se que todos os registros do arquivo contém o caracter de fim de linha conforme especificado no layout');
end;

function TACBrNFSeProviderBarueriErros.Causa(const ACodigo: String): String;
//var
//  S: TStringList;
begin
  try
    Result := Copy(Values[ACodigo], 1, Pos('|', Values[ACodigo])-1);
  except
    Result := '';
  end;
 {
    S := TStringList.Create;
  try
    S.Delimiter := '|';
//não é compativel com o D7    S.StrictDelimiter := True;
    S.DelimitedText := Values[ACodigo];
    Result := S[0];
  except
    Result := '';
  end;
  S.Free;
}
end;

function TACBrNFSeProviderBarueriErros.Solucao(const ACodigo: String): String;
//var
//  S: TStringList;
begin
  try
    Result := Copy(Values[ACodigo], Pos('|', Values[ACodigo])+1, Length(Values[ACodigo]));
  except
    Result := '';
  end;
 {
  S := TStringList.Create;
  try
    S.Delimiter := '|';
//não é compativel com o D7    S.StrictDelimiter := True;
    S.DelimitedText := Values[ACodigo];
    Result := S[1];
  except
    Result := '';
  end;
  S.Free;
  }
end;

{ TACBrNFSeXWebserviceISSBarueri }

function TACBrNFSeXWebserviceISSBarueri.Recepcionar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:NFeLoteEnviarArquivo>';
  Request := Request + '<nfe:VersaoSchema>1</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>';
  Request := Request + IncluirCDATA(AMSG);
  Request := Request + '</nfe:MensagemXML>';
  Request := Request + '</nfe:NFeLoteEnviarArquivo>';

  Result := Executar('http://www.barueri.sp.gov.br/nfe/NFeLoteEnviarArquivo',
    Request,
    ['NFeLoteEnviarArquivoResult'],
    ['xmlns:nfe="http://www.barueri.sp.gov.br/nfe"']
  );
end;

function TACBrNFSeXWebserviceISSBarueri.ConsultarSituacao(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:NFeLoteStatusArquivo>';
  Request := Request + '<nfe:VersaoSchema>1</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>';
  Request := Request + IncluirCDATA(AMSG);
  Request := Request + '</nfe:MensagemXML>';
  Request := Request + '</nfe:NFeLoteStatusArquivo>';

  Result := Executar('http://www.barueri.sp.gov.br/nfe/NFeLoteStatusArquivo',
    Request,
    ['NFeLoteStatusArquivoResult'],
    ['xmlns:nfe="http://www.barueri.sp.gov.br/nfe"']
  );
end;

function TACBrNFSeXWebserviceISSBarueri.ConsultarLote(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:NFeLoteBaixarArquivo>';
  Request := Request + '<nfe:VersaoSchema>1</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>';
  Request := Request + IncluirCDATA(AMSG);
  Request := Request + '</nfe:MensagemXML>';
  Request := Request + '</nfe:NFeLoteBaixarArquivo>';

  Result := Executar('http://www.barueri.sp.gov.br/nfe/NFeLoteBaixarArquivo',
    Request,
    ['NFeLoteBaixarArquivoResult'],
    ['xmlns:nfe="http://www.barueri.sp.gov.br/nfe"']
  );
end;

function TACBrNFSeXWebserviceISSBarueri.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:NFeLoteEnviarArquivo>';
  Request := Request + '<nfe:VersaoSchema>1</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>';
  Request := Request + IncluirCDATA(AMSG);
  Request := Request + '</nfe:MensagemXML>';
  Request := Request + '</nfe:NFeLoteEnviarArquivo>';

  Result := Executar('http://www.barueri.sp.gov.br/nfe/NFeLoteEnviarArquivo',
    Request,
    ['NFeLoteEnviarArquivoResult'],
    ['xmlns:nfe="http://www.barueri.sp.gov.br/nfe"']
  );
end;

{ TACBrNFSeProviderISSBarueri }

function TACBrNFSeProviderISSBarueri.ExisteErroRegistro(const ALinha: String): Boolean;
begin
  Result := (Length(ALinha) > 1971) and (
    (ALinha[1] = '1') or
    (ALinha[1] = '2') or
    (ALinha[1] = '3') or
    (ALinha[1] = '9')
  );
end;

procedure TACBrNFSeProviderISSBarueri.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    UseCertificateHTTP := True;
    ModoEnvio := meLoteAssincrono;
    ConsultaNFSe := False;
  end;

  with ConfigMsgDados do
  begin
    Prefixo := 'nfe';
    LoteRps.DocElemento := 'NFeLoteEnviarArquivo';
    LoteRps.xmlns := 'http://www.barueri.sp.gov.br/nfe';
    ConsultarSituacao.DocElemento := 'NFeLoteStatusArquivo';
    ConsultarSituacao.xmlns := 'http://www.barueri.sp.gov.br/nfe';
    ConsultarLote.DocElemento := 'NFeLoteBaixarArquivo';
    ConsultarLote.xmlns := 'http://www.barueri.sp.gov.br/nfe';
    GerarNSLoteRps := False;
    GerarPrestadorLoteRps := False;
    UsarNumLoteConsLote := False;
  end;

  SetXmlNameSpace('http://www.barueri.sp.gov.br/nfe');
  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderISSBarueri.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSBarueri.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSBarueri.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSBarueri.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSBarueri.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSBarueri.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create('ERR_SEM_URL');
end;

procedure TACBrNFSeProviderISSBarueri.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;
  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit;
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);
      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno');
      Response.Sucesso := (Response.Erros.Count = 0);
      ANode := Document.Root;
      Response.Data := Now;
      Response.Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('ProtocoloRemessa'), tcStr);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderISSBarueri.GerarMsgDadosEmitir(Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  XML, NumeroRps: String;
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  NumeroRps := TACBrNFSeX(FAOwner).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero;

  if (EstaVazio(NumeroRps)) then
    NumeroRps := FormatDateTime('yyyymmddzzz', Now);

  XML := '<NFeLoteEnviarArquivo xmlns="http://www.barueri.sp.gov.br/nfe">';
  XML := XML + '<InscricaoMunicipal>' + Emitente.InscMun + '</InscricaoMunicipal>';
  XML := XML + '<CPFCNPJContrib>' + Emitente.CNPJ + '</CPFCNPJContrib>';
  XML := XML + '<NomeArquivoRPS>' + Format('Rps-0%s.txt', [NumeroRps]) + '</NomeArquivoRPS>';
  XML := XML + '<ApenasValidaArq>false</ApenasValidaArq>';
  XML := XML + '<ArquivoRPSBase64>' + string(EncodeBase64(AnsiString(Params.Xml))) + '</ArquivoRPSBase64>';
  XML := XML + '</NFeLoteEnviarArquivo>';

  Response.ArquivoEnvio := XML;
end;
function TACBrNFSeProviderISSBarueri.PrepararRpsParaLote(const aXml: string): string;
begin
  Result := aXml;
end;

procedure TACBrNFSeProviderISSBarueri.PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XML: String;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := Desc101;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  XML := '<NFeLoteStatusArquivo xmlns="http://www.barueri.sp.gov.br/nfe">';
  XML := XML + '<InscricaoMunicipal>' + Emitente.InscMun + '</InscricaoMunicipal>';
  XML := XML + '<CPFCNPJContrib>' + Emitente.CNPJ + '</CPFCNPJContrib>';
  XML := XML + '<ProtocoloRemessa>' + Response.Protocolo + '</ProtocoloRemessa>';
  XML := XML + '</NFeLoteStatusArquivo>';

  Response.ArquivoEnvio := XML;
end;

procedure TACBrNFSeProviderISSBarueri.TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit;
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;
      AuxNode := ANode.Childrens.FindAnyNs('ListaNfeArquivosRPS');

      if (AuxNode = Nil) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit;
      end;

      Response.Data := Iso8601ToDateTime(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEnvioArq'), tcStr));
      Response.NumeroRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoRemessa'), tcStr);
      Response.Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NomeArqRetorno'), tcStr);
      Response.Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('SituacaoArq'), tcStr);

      if (Response.Situacao = '-2') then
        Response.DescSituacao := 'Aguardando Processamento'
      else if (Response.Situacao = '-1') then
        Response.DescSituacao := 'Em Processamento'
      else if (Response.Situacao = '0') then
        Response.DescSituacao := 'Arquivo Validado'
      else if (Response.Situacao = '1') then
        Response.DescSituacao := 'Arquivo Importado'
      else if (Response.Situacao = '2') then
        Response.DescSituacao := 'Arquivo com Erros';
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderISSBarueri.PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XML, NumeroRps, xRps: String;
  Nota: TNotaFiscal;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
  end;
  {
  if (Response.InfCancelamento.DataEmissaoNFSe <= 0) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod122;
    AErro.Descricao := Desc122;
  end;
  }
  if EstaVazio(Response.InfCancelamento.CodCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod109;
    AErro.Descricao := Desc109;
  end;

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := Desc110;
  end;

  if (TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod002;
    AErro.Descricao := Desc002;
  end;

  if (Response.Erros.Count > 0) then
  begin
    Response.Sucesso := False;
    Exit;
  end;

  Nota := Nil;
  NumeroRps := '';

  if (Response.InfCancelamento.NumeroRps > 0) then
  begin
    NumeroRps := IntToStr(Response.InfCancelamento.NumeroRps);
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumeroRps);
  end;

  if (EstaVazio(NumeroRps)) then
  begin
    NumeroRps := Response.InfCancelamento.NumeroNFSe;
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumeroRps);
  end;

  if (Nota = Nil) then
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[0];

  Nota.NFSe.StatusRps := srCancelado;
  Nota.NFSe.MotivoCancelamento := Response.InfCancelamento.MotCancelamento;
  Nota.NFSe.CodigoCancelamento := Response.InfCancelamento.CodCancelamento;
  Nota.GerarXML;

  Nota.XmlRps := AplicarXMLtoUTF8(Nota.XmlRps);
  Nota.XmlRps := AplicarLineBreak(Nota.XmlRps, '');

  SalvarXmlRps(Nota);

  xRps := RemoverDeclaracaoXML(Nota.XmlRps);
  xRps := PrepararRpsParaLote(xRps);

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  XML := '<NFeLoteEnviarArquivo xmlns="http://www.barueri.sp.gov.br/nfe">';
  XML := XML + '<InscricaoMunicipal>' + Emitente.InscMun + '</InscricaoMunicipal>';
  XML := XML + '<CPFCNPJContrib>' + Emitente.CNPJ + '</CPFCNPJContrib>';
  XML := XML + '<NomeArquivoRPS>' + Format('Rps-Canc-%s.txt', [NumeroRps]) + '</NomeArquivoRPS>';
  XML := XML + '<ApenasValidaArq>false</ApenasValidaArq>';
  XML := XML + '<ArquivoRPSBase64>' + string(EncodeBase64(AnsiString(xRps))) + '</ArquivoRPSBase64>';
  XML := XML + '</NFeLoteEnviarArquivo>';

  Response.ArquivoEnvio := XML;
end;

procedure TACBrNFSeProviderISSBarueri.TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit;
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;
      Response.Data := Now;
      Response.Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('ProtocoloRemessa'), tcStr);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderISSBarueri.PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XML: String;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := Desc101;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  XML := '<NFeLoteBaixarArquivo xmlns="http://www.barueri.sp.gov.br/nfe">';
  XML := XML + '<InscricaoMunicipal>' + Emitente.InscMun + '</InscricaoMunicipal>';
  XML := XML + '<CPFCNPJContrib>' + Emitente.CNPJ + '</CPFCNPJContrib>';
  XML := XML + '<NomeArqRetorno>' + Response.Protocolo + '</NomeArqRetorno>';
  XML := XML + '</NFeLoteBaixarArquivo>';

  Response.ArquivoEnvio := XML;
end;

procedure TACBrNFSeProviderISSBarueri.TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  ArquivoBase64: String;
  Dados: TStringList;
  NumRps: String;
  ListaErros: TACBrNFSeProviderBarueriErros;
  Erros: TStringList;
  ANota: TNotaFiscal;
  I, X: Integer;
  XML: string;
begin
  Document := TACBrXmlDocument.Create;
  Dados := TStringList.Create;
  ListaErros := TACBrNFSeProviderBarueriErros.Create;
  Erros := TStringList.Create;
  Erros.Delimiter := ';';

  try
    try
      if EstaVazio(Response.ArquivoRetorno) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit;
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;
      ArquivoBase64 := ObterConteudoTag(ANode.Childrens.FindAnyNs('ArquivoRPSBase64'), tcStr);

      if (EstaVazio(ArquivoBase64) and (not ACBrUtil.Strings.StrIsBase64(ArquivoBase64))) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit;
      end;

      {$IFDEF FPC}
      Dados.LineBreak := CRLF;
      {$ELSE}
        {$IFDEF DELPHI2006_UP}
        Dados.LineBreak := CRLF;
        {$ENDIF}
      {$ENDIF}

      Dados.Text := string(DecodeBase64(AnsiString(ArquivoBase64)));

      for I := 0 to Pred(Dados.Count) do
      begin
        if (ExisteErroRegistro(Dados[I])) then
        begin
          // A partir do caractere 1971, é a listagem de codigo de erros
          Erros.DelimitedText := Trim(Copy(Dados[I], 1971, Length(Dados[I])));

          for X := 0 to Pred(Erros.Count) do
          begin
            if NaoEstaVazio(Erros[X]) then
            begin
              AErro := Response.Erros.New;
              AErro.Codigo := Erros[X];
              AErro.Descricao := ListaErros.Causa(Erros[X]);
              AErro.Correcao := ListaErros.Solucao(Erros[X]);
            end;
          end;
        end;
      end;

      Response.Sucesso := (Response.Erros.Count = 0);

      NumRps := Trim(Copy(Dados[0], Pos('PMB002', Dados[0]), Length(Dados[0])));
      NumRps := StringReplace(NumRps, 'PMB002', '', [rfReplaceAll]);
      Response.NumeroRps := NumRps;

      //Retorno do Txt de um RPS Processado com sucesso...
      if ( (Response.Sucesso) and (Length(Dados[0]) > 26) ) then
      begin
        //1XXXXXXX0000000000000000PMB00200000000004
        //2     00000120220318211525723K.1473.0553.5240699-I...
        //Onde 723K.1473.0553.5240699-I é o Codigo de verificacao para fazer download do XML
        Response.Situacao := '1';
        Response.DescSituacao := 'Arquivo Importado';
        Response.Protocolo := Trim(Copy(Dados[1], 27, 24));
        Response.NumeroNota := Trim(Copy(Dados[1], 7, 6));
        Response.SerieRps := Trim(Copy(Dados[1], 51, 4));
        Response.SerieNota := Trim(Copy(Dados[1], 2, 5));

        if NaoEstaVazio(Trim(Copy(Dados[1], 22, 6))) then
        begin
          Response.Data := EncodeDataHora(Trim(Copy(Dados[1], 13, 8)), 'YYYYMMDD');
          Response.Data := Response.Data + StrToTime(Format('%S:%S:%S', [Trim(Copy(Dados[1], 21, 2)), Trim(Copy(Dados[1], 23, 2)), Trim(Copy(Dados[1], 25, 2))]));
        end
        else
          Response.Data := EncodeDataHora(Trim(Copy(Dados[1], 13, 8)), 'YYYYMMDD');

        if (FAOwner.Configuracoes.WebServices.AmbienteCodigo = 1) then
        begin
          Response.Link := 'https://www.barueri.sp.gov.br/nfe/xmlNFe.ashx?codigoautenticidade=' + Response.Protocolo + '&numdoc=' + Trim(Copy(Dados[1], 94, 14));
        end;

        if NaoEstaVazio(Response.Link) then
        begin
          XML := string(FAOwner.SSL.HTTPGet(Response.Link));

          if (NaoEstaVazio(XML) and (Pos('<ConsultarNfeServPrestadoResposta', XML) > 0) ) then
          begin
            XML := RemoverDeclaracaoXML(XML);
            XML := ConverteXMLtoUTF8(XML);
            Document.Clear();
            Document.LoadFromXml(XML);
            Dados.Clear;
            Dados.Text := XML;
          end;
        end;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        ANota := CarregarXmlNfse(ANota, Dados.Text);
        SalvarXmlNfse(ANota);
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
    FreeAndNil(Dados);
    FreeAndNil(Erros);
    FreeAndNil(ListaErros);
  end;
end;

procedure TACBrNFSeProviderISSBarueri.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag: string; const AMessageTag: string);
var
  Codigo: String;
  ANode: TACBrXmlNode;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = Nil) then Exit;
  Codigo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Codigo'), tcStr);

  if (Codigo <> 'OK200') then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Codigo'), tcStr);
    AErro.Descricao := ObterConteudoTag(ANode.Childrens.FindAnyNs('Mensagem'), tcStr);
    AErro.Correcao := ObterConteudoTag(ANode.Childrens.FindAnyNs('Correcao'), tcStr);
  end;
  {
    As tag que contem o código, mensagem e correção do erro são diferentes do padrão
    <NFeLoteEnviarArquivoResult>
        <ListaMensagemRetorno>
            <Codigo>OK200</Codigo>
            <Mensagem>Procedimento executado com sucesso - Arquivo agendado para processamento: RPS-1.txt</Mensagem>
            <Correcao/>
        </ListaMensagemRetorno>
        <ProtocoloRemessa>ENV555318486B20220316155838</ProtocoloRemessa>
    </NFeLoteEnviarArquivoResult>
    <ListaMensagemRetorno>
        <Codigo>E0001</Codigo>
        <Mensagem>Certificado digital inválido ou não informado</Mensagem>
        <Correcao>Informe um certificado válido padrão ICP-Brasil</Correcao>
    </ListaMensagemRetorno>
  }
end;

function TACBrNFSeProviderISSBarueri.AplicarXMLtoUTF8(AXMLRps: String): String;
begin
  Result := string(NativeStringToUTF8(AXMLRps));
end;

function TACBrNFSeProviderISSBarueri.AplicarLineBreak(AXMLRps: String; const ABreak: String): String;
begin
  Result := AXMLRps;
end;

end.

