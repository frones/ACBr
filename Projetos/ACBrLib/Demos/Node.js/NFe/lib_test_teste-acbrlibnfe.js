// {******************************************************************************}
// { Projeto: Componentes ACBr                                                    }
// {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
// { mentos de Automação Comercial utilizados no Brasil                           }
// {                                                                              }
// { Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
// {                                                                              }
// { Colaboradores nesse arquivo: Fábio Francisco - Centrodata Sistemas,          }
// {   Filipe Natividade - Centrodata Sistemas,                                   }
//     Fernanda de Souza Alves - C E S Consultoria e Sistemas,                    }
//     Débora Bonfim Guedes - C E S Consultoria e Sistemas	                      }
// {                                                                              }
// {  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
// { Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
// {                                                                              }
// {  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
// { sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
// { Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
// { qualquer versão posterior.                                                   }
// {                                                                              }
// {  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
// { NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
// { ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
// { do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
// {                                                                              }
// {  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
// { com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
// { no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
// { Você também pode obter uma copia da licença em:                              }
// { http://www.opensource.org/licenses/lgpl-license.php                          }
// {                                                                              }
// { Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
// {       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
// {******************************************************************************}

const path = require('path');

const ffi = require('ffi-napi');
const ref = require('ref-napi');

var pathDllACBrLibNFe = path.join(__dirname, 'ACBrNFe64.dll');
var pathXML = path.join(__dirname, 'chave-nfe.xml');

var eArqConfig = path.join(__dirname, 'acbrlib.ini');
var eChaveCrypt = '';

async function getNFe() {
  var libm = ffi.Library(pathDllACBrLibNFe, {

    //Métodos da Biblioteca

    // NFE_Inicializar([eArqConfig, eChaveCrypt]);
    NFE_Inicializar: ['int', ['string', 'string']],
    // NFE_Finalizar();
    NFE_Finalizar: ['int', []],
    // NFE_UltimoRetorno(sMensagem, ref esTamanho);
    NFE_UltimoRetorno: ['int', ['string', 'string']],
    // NFE_Nome(sNome, ref esTamanho);
    NFE_Nome: ['int',['string','string']],
    // NFE_Versao(sVersao, ref esTamanho);
    NFE_Versao: ['int', ['string','string']],

    //Métodos de Configuração

    // NFE_ConfigLer([eArqConfig]);
    NFE_ConfigLer: ['int', ['string']],
    // NFE_ConfigGravar([eArqConfig]);
    NFE_ConfigGravar: ['int', ['string']],
    // NFE_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
    NFE_ConfigLerValor: ['int',['string','string','string','string']],
    // NFE_ConfigGravarValor(eSessao, eChave, sValor);
    NFE_ConfigGravarValor: ['int', ['string', 'string', 'string']],
    // NFE_ConfigImportar([eArqConfig]);
    NFE_ConfigImportar: ['int',['string']],
    // NFE_ConfigExportar(sMensagem, ref esTamanho);
    NFE_ConfigExportar: ['int',['string','string']],

    //Métodos NFe

    // NFE_CarregarXML(eArquivoOuXML);
    NFE_CarregarXML: ['int', ['string']], 
    // NFE_CarregarINI(eArquivoOuINI);
    NFE_CarregarINI: ['int', ['string']],
    // NFE_ObterXml(AIndex, sResposta, esTamanho);
    NFE_ObterXml: ['int',['int', 'string', 'string']],
    // NFE_GravarXml(AIndex, [eNomeArquivo], [ePathArquivo]);
    NFE_GravarXml: ['int', ['int','string','string']],
    // NFE_ObterIni(AIndex, sResposta, esTamanho);
    NFE_ObterIni: ['int', ['int','string','string']],
    // NFE_GravarIni(AIndex, eNomeArquivo, [ePathArquivo]);
    NFE_GravarIni: ['int',['int', 'string', 'string']],
    // NFE_CarregarEventoXML(eArquivoOuXML);
    NFE_CarregarEventoXML: ['int',['string']],
    // NFE_CarregarEventoINI(eArquivoOuINI);
    NFE_CarregarEventoINI: ['int',['string']],
    // NFE_LimparLista();
    NFE_LimparLista: ['int',[]],
    // NFE_LimparListaEventos();
    NFE_LimparListaEventos: ['int',[]],
    // NFE_Assinar();
    NFE_Assinar: ['int', []],
    // NFE_Validar();
    NFE_Validar: ['int', []],
    // NFE_ValidarRegrasdeNegocios(sResposta, esTamanho);
    NFE_ValidarRegrasdeNegocios: ['int',['string','string']],
    // NFE_VerificarAssinatura(sResposta, esTamanho);
    NFE_VerificarAssinatura: ['int',['string','string']],
    // NFE_GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, AEmissao, ACNPJCPF, sResposta, esTamanho);
    NFE_GerarChave: ['int',['int','int','int','int','int','int','string','string','string','string']],
    // NFE_ObterCertificados(sResposta, esTamanho);
    NFE_ObterCertificados: ['int',['string','string']],
    // NFE_GetPath(ATipo, sResposta, esTamanho);
    NFE_GetPath: ['int',['int','string','string']],
    // NFE_GetPathEvento(ACodEvento, sResposta, esTamanho);
    NFE_GetPathEvento: ['int',['int','string','string']],
    // NFE_StatusServico(sResposta, esTamanho);
    NFE_StatusServico: ['int', ['status','string']],
    // NFE_Consultar( eChaveOuNFe, AExtrairEventos, sResposta, esTamanho);
    NFE_Consultar: ['int',['string','bool','string','string']],
    // NFE_ConsultarRecibo(ARecibo, sResposta, esTamanho);
    NFE_ConsultarRecibo: ['int',['string','string','string']],
    // NFE_ConsultaCadastro(cUF, nDocumento, nIE, sResposta, esTamanho);
    NFE_ConsultaCadastro: ['int',['string','string','bool','string','string']],
    // NFE_Inutilizar(ACNPJ, AJustificativa, Ano, Modelo, Serie, NumeroInicial, NumeroFinal, sResposta, esTamanho);
    NFE_Inutilizar: ['int',['string','string','int','int','int','int','int','string','string']],
    // NFE_Enviar( (ALote, AImprimir, ASincrono, AZipado, sResposta, esTamanho);
    NFE_Enviar: ['int', ['int','bool','bool','bool','string','string']],
    // NFE_Cancelar(eChave, eJustificativa, eCNPJ, ALote, sResposta, esTamanho);
    NFE_Cancelar: ['int',['string','string','string','int','string','string']],
    // NFE_EnviarEvento(idLote, sResposta, esTamanho);
    NFE_EnviarEvento: ['int',['int','string','string']],
    // NFE_DistribuicaoDFePorUltNSU(AcUFAutor, eCNPJCPF, eultNSU, sResposta, esTamanho);
    NFE_DistribuicaoDFePorUltNSU: ['int',['int','string','string','string','string']],
    // NFE_DistribuicaoDFePorNSU(AcUFAutor, eCNPJCPF, eNSU, sResposta, esTamanho);
    NFE_DistribuicaoDFePorNSU: ['int',['int','string','string','string','string']],
    // NFE_DistribuicaoDFePorChave(AcUFAutor, eCNPJCPF, eChave, sResposta, esTamanho);
    NFE_DistribuicaoDFePorChave: ['int',['int','string','string','string','string']],
    // NFE_EnviarEmail(ePara, eXMLNFe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem);
    NFE_EnviarEmail: ['int',['string','string','bool','string','string','string','string']],
    // NFE_EnviarEmailEvento(ePara, eChaveEvento, eChaveNFe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem);
    NFE_EnviarEmailEvento: ['int',['string','string','string','bool','string','string','string','string']],
    // NFE_Imprimir([cImpressora], [nNumCopias], [cProtocolo], [bMostrarPreview], [cMarcaDagua], [bViaConsumidor], [bSimplificado]);
    NFE_Imprimir: ['int',['string','int','string','string','string','string','string',]],
    // NFE_ImprimirPDF();
    NFE_ImprimirPDF: ['int', []],
    // NFE_SalvarPDF(sResposta, esTamanho);
    NFE_SalvarPDF: ['int',['string','string']],
    // NFE_ImprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento);
    NFE_ImprimirEvento: ['int',['string','string']],
    // NFE_ImprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento);
    NFE_ImprimirEventoPDF: ['int',['string','string']],
    // NFE_SalvarEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento);
    NFE_SalvarEventoPDF: ['int',['string','string']],
    // NFE_ImprimirInutilizacao(eArquivoXml);
    NFE_ImprimirInutilizacao: ['int', ['string']],
    // NFE_ImprimirInutilizacaoPDF(eArquivoXml);
    NFE_ImprimirInutilizacaoPDF: ['int',['string']],
    // NFE_SalvarInutilizacaoPDF(eArquivoXml);
    NFE_SalvarInutilizacaoPDF: ['int',['string']],     
  });

  var inicio = 2;
  const buflength = 256;

  let aloc_sResposta = Buffer.alloc(buflength);
  let aloc_esTamanho = ref.alloc('int', buflength);

  inicio = libm.NFE_Inicializar(eArqConfig, eChaveCrypt);
  console.log(`iniciou >>>>>>> ${inicio}`);

  inicio = libm.NFE_ConfigGravarValor('DFe', 'ArquivoPFX', path.join(path.resolve(__dirname, 'certificado'), 'nome_do_certificado.pfx'));
  inicio = libm.NFE_ConfigGravarValor('DFe', 'Senha', '1234');
  inicio = libm.NFE_ConfigGravarValor('NFe', 'PathSchemas', path.resolve(__dirname, 'Schemas', 'NFe'));
  console.log(`Set Configurações ${inicio}`);

  inicio = libm.NFE_CarregarXML(pathXML);
  console.log(`carregar xml >>>>>>> ${inicio}`);

  inicio = libm.NFE_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
  console.log(`ultmio retorno >>>>>>>> ${inicio}`);
  console.log(`Mensagem Ultimo Retorno: `, aloc_sResposta.toString());

  inicio = libm.NFE_Assinar();
  console.log(`assinar xml >>>>>>> ${inicio}`);

  aloc_sResposta = Buffer.alloc(buflength);
  aloc_esTamanho = ref.alloc('int', buflength);

  inicio = libm.NFE_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
  console.log(`ultimo retorno apos assinar >>>>>>>> ${inicio}`);
  console.log(`Mensagem: `, aloc_sResposta.toString('ascii'));

  inicio = libm.NFE_Validar();
  console.log(`validar xml >>>>>>> ${inicio}`);


  aloc_sResposta = Buffer.alloc(buflength);
  aloc_esTamanho = ref.alloc('int', buflength);

  inicio = libm.NFE_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
  console.log(`ultmio retorno apos validar >>>>>>>> ${inicio}`);
  console.log(`Mensagem: `, aloc_sResposta.toString());

  inicio = libm.NFE_Finalizar();
  console.log(`finalizar >>>>>>>> ${inicio}`);
}

getNFe();