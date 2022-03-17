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
    NFE_Inicializar: ['int', ['string', 'string']],
    NFE_ConfigLer: ['int', ['string']],
    NFE_CarregarINI: ['int', ['string']],
    NFE_CarregarXML: ['int', ['string']],
    NFE_UltimoRetorno: ['int', ['string', 'string']],
    NFE_ImprimirPDF: ['int', []],
    NFE_Finalizar: ['int', []],
    NFE_Assinar: ['int', []],
    NFE_Validar: ['int', []],
    NFE_ConfigGravarValor: ['int', ['string', 'string', 'string']],
  });

  var inicio = 2;
  const buflength = 256;

  let aloc_sResposta = Buffer.alloc(buflength);
  let aloc_esTamanho = ref.alloc('int', buflength);

  inicio = libm.NFE_Inicializar(eArqConfig, eChaveCrypt);
  console.log('iniciou >>>>>>> ${inicio}');

  inicio = libm.NFE_ConfigGravarValor('DFe', 'ArquivoPFX', path.join(path.resolve(__dirname, 'certificado'), 'nome_do_certificado.pfx'));
  inicio = libm.NFE_ConfigGravarValor('DFe', 'Senha', '1234');
  inicio = libm.NFE_ConfigGravarValor('NFe', 'PathSchemas', path.resolve(__dirname, 'Schemas', 'NFe'));
  console.log('Set Configurações ${inicio}');

  inicio = libm.NFE_CarregarXML(pathXML);
  console.log('carregar xml >>>>>>> ${inicio}');

  inicio = libm.NFE_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
  console.log('ultmio retorno >>>>>>>> ${inicio}');
  console.log('Mensagem Ultimo Retorno: ', aloc_sResposta.toString());

  inicio = libm.NFE_Assinar();
  console.log('assinar xml >>>>>>> ${inicio}');

  aloc_sResposta = Buffer.alloc(buflength);
  aloc_esTamanho = ref.alloc('int', buflength);

  inicio = libm.NFE_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
  console.log('ultmio retorno apos assinar >>>>>>>> ${inicio}');
  console.log('Mensagem: ', aloc_sResposta.toString('ascii'));

  inicio = libm.NFE_Validar();
  console.log('validar xml >>>>>>> ${inicio}');


  aloc_sResposta = Buffer.alloc(buflength);
  aloc_esTamanho = ref.alloc('int', buflength);

  inicio = libm.NFE_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
  console.log('ultmio retorno apos validar >>>>>>>> ${inicio}');
  console.log('Mensagem: ', aloc_sResposta.toString());

  inicio = libm.NFE_Finalizar();
  console.log('finalizar >>>>>>>> ${inicio}');
}

getNFe();