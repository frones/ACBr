// {******************************************************************************}
// { Projeto: Componentes ACBr                                                    }
// {  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
// { mentos de Automação Comercial utilizados no Brasil                           }
// {                                                                              }
// { Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
// {                                                                              }
// { Colaboradores nesse arquivo: Renato Rubinho                                  }
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

var pathDllACBrLibReinf = path.join(__dirname, 'ACBrReinf32.dll');
var eArqConfig = path.join(__dirname, 'acbrlib.ini');
var eChaveCrypt = '';

async function getReinf() {
  var libm = ffi.Library(pathDllACBrLibReinf, {

    // Métodos da Biblioteca

    // Reinf_Inicializar([eArqConfig, eChaveCrypt]);
    Reinf_Inicializar: ['int', ['string', 'string']],
    // Reinf_Finalizar();
    Reinf_Finalizar: ['int', []],
    // Reinf_UltimoRetorno(sMensagem, ref esTamanho);
    Reinf_UltimoRetorno: ['int', ['string', 'string']],
    // Reinf_Nome(sNome, ref esTamanho);
    Reinf_Nome: ['int',['string','string']],
    // Reinf_Versao(sVersao, ref esTamanho);
    Reinf_Versao: ['int', ['string','string']],

    // Métodos de Configuração

    // Reinf_ConfigLer([eArqConfig]);
    Reinf_ConfigLer: ['int', ['string']],
    // Reinf_ConfigGravar([eArqConfig]);
    Reinf_ConfigGravar: ['int', ['string']],
    // Reinf_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
    Reinf_ConfigLerValor: ['int',['string','string','string','string']],
    // Reinf_ConfigGravarValor(eSessao, eChave, sValor);
    Reinf_ConfigGravarValor: ['int', ['string', 'string', 'string']],
    // Reinf_ConfigImportar([eArqConfig]);
    Reinf_ConfigImportar: ['int',['string']],
    // Reinf_ConfigExportar(sMensagem, ref esTamanho);
    Reinf_ConfigExportar: ['int',['string','string']],

    // Métodos Reinf

    // Reinf_CriarEventoReinf(eArqIni);
    Reinf_CriarEventoReinf: ['int', ['string']],
    // Reinf_EnviarReinf(aGrupo, sResposta, esTamanho);
    Reinf_EnviarReinf: ['int',['string','string','string']],
    // Reinf_ConsultarReinf(eProtocolo, sResposta, esTamanho);
    Reinf_ConsultarReinf: ['int',['string','string','string']],
    // Reinf_ConsultarReciboReinf(ePerApur, aTipoEvento, eNrInscEstab, eCnpjPrestador, 
    //   eNrInscTomador, eDtApur, eCpfCnpjBenef, eCnpjFonte, sResposta, esTamanho);
    Reinf_ConsultarReciboReinf: ['int',['string','int','string','string',
      'string','string','string','string','string','string']],
    // Reinf_CriarEnviarReinf(eArqIni, aGrupo);
    Reinf_CriarEnviarReinf: ['int',['string','string']],
    // Reinf_LimparReinf();
    Reinf_LimparReinf: ['int', []],
    // Reinf_CarregarXMLEventoReinf(eArquivoOuXML);
    Reinf_CarregarXMLEventoReinf: ['int',['string']],
    // Reinf_SetIdContribuinte(aIdContribuinte);
    Reinf_SetIdContribuinte: ['int',['string']],
    // Reinf_SetIDTransmissor(aIdTransmissor);
    Reinf_SetIDTransmissor: ['int',['string']],
    // Reinf_SetTipoContribuinte(aTipoContribuinte);
    Reinf_SetTipoContribuinte: ['int',['string']],
    // Reinf_SetVersaoDF(sVersao);
    Reinf_SetVersaoDF: ['int',['string']],
    // Reinf_ObterCertificados(sResposta, esTamanho);
    Reinf_ObterCertificados: ['int',['string','string']],
    // Reinf_Validar();
    Reinf_Validar: ['int', []],
  });

  try 
  {
    var inicio = 2;
    var processo = 'inicio';
    var paramStr01 = '';
    const buflength = 4096;
  
    let aloc_sResposta = Buffer.alloc(buflength);
    let aloc_esTamanho = ref.alloc('int', buflength);
  
    processo = 'Reinf_Inicializar';
    inicio = libm.Reinf_Inicializar(eArqConfig, eChaveCrypt);
    console.log(processo + ` >>>>>>> ${inicio}`);
  
    processo = 'Configurando';
    inicio = libm.Reinf_ConfigGravarValor('DFe', 'ArquivoPFX', path.join(__dirname, 'nome_do_certificado.pfx'));
    inicio = libm.Reinf_ConfigGravarValor('DFe', 'Senha', '1234');
    
    // VersaoDF (4)v1_05_01 (6)v2_01_02
    paramStr01 = '6';
    inicio = libm.Reinf_ConfigGravarValor('Reinf', 'VersaoDF', paramStr01);
    console.log(`VersaoDF >>>>>>> ${paramStr01}`);
  
    inicio = libm.Reinf_ConfigGravarValor('Reinf', 'Ambiente', '1');
    inicio = libm.Reinf_ConfigGravarValor('Reinf', 'SalvarArq', '1');
    inicio = libm.Reinf_ConfigGravarValor('Reinf', 'SalvarGer', '1');
    inicio = libm.Reinf_ConfigGravarValor('Reinf', 'SalvarWS', '1');
    inicio = libm.Reinf_ConfigGravarValor('Reinf', 'Timeout', '30000');
  
    inicio = libm.Reinf_ConfigGravarValor('Reinf', 'PathSchemas', path.resolve(__dirname, 'Schemas', 'Reinf'));
    inicio = libm.Reinf_ConfigGravarValor('Reinf', 'PathSalvar', path.resolve(__dirname, 'Retorno'));
    inicio = libm.Reinf_ConfigGravarValor('Reinf', 'PathReinf', path.resolve(__dirname, 'Retorno'));
  
    console.log(`Set Configurações ${inicio}`);
  
    aloc_sResposta = Buffer.alloc(buflength);
    aloc_esTamanho = ref.alloc('int', buflength);
  
    //* Exemplo de Criacao e Envio
    processo = 'Reinf_LimparReinf';
    inicio = libm.Reinf_LimparReinf();
    console.log(processo + ` >>>>>>> ${inicio}`);
    inicio = libm.Reinf_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
    console.log(processo + ' - Ultimo Retorno: ', aloc_sResposta.toString());
  
    processo = 'Reinf_CriarEventoReinf';
    paramStr01 = path.join(__dirname, 'evento.ini'); // Ini do evento salvo na pasta do projeto
    inicio = libm.Reinf_CriarEventoReinf(paramStr01);
    console.log(processo + ` evento >>>>>>> ${inicio}`);
    inicio = libm.Reinf_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
    console.log(processo + ' - Ultimo Retorno: ', aloc_sResposta.toString());
  
    processo = 'Reinf_EnviarReinf';
    paramStr01 = '';  // Grupo
    inicio = libm.Reinf_EnviarReinf(paramStr01, aloc_sResposta, aloc_esTamanho);
    console.log(processo + ` >>>>>>>> ${inicio}`);
    inicio = libm.Reinf_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
    console.log(processo + ' - Ultimo Retorno: ', aloc_sResposta.toString());
    //*/
    /* Exemplo de Consulta de Protocolo
    processo = 'Reinf_ConsultarReinf';
    paramStr01 = '2.202310.1234567'; // Numero do Protocolo
    inicio = libm.Reinf_ConsultarReinf(paramStr01, aloc_sResposta, aloc_esTamanho);
    console.log(processo + ` >>>>>>>> ${inicio}`);
    inicio = libm.Reinf_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
    console.log(processo + ' - Ultimo Retorno: ', aloc_sResposta.toString());
    //*/
    /* Exemplo de Consulta de recibo
    processo = 'Reinf_ConsultarReciboReinf';
    inicio = libm.Reinf_ConsultarReciboReinf('2023-09',1000,'12345678000195',
      '98765432000198','12345678000195','30/09/2023', '', '',
      aloc_sResposta, aloc_esTamanho);
    console.log(processo + ` >>>>>>>> ${inicio}`);
    inicio = libm.Reinf_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
    console.log(processo + ' - Ultimo Retorno: ', aloc_sResposta.toString());
    //*/

    processo = 'Reinf_Finalizar';
    inicio = libm.Reinf_Finalizar();
    console.log(processo + ` >>>>>>>> ${inicio}`);
  } 
    catch(error) 
  {
    console.error('Erro no processando [ ' + processo + ' ]:', error)
  }
}

getReinf();