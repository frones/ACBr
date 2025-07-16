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
const koffi = require('koffi');
const os = require('os');
const TAMANHO_BUFFER = 1024;

var pathDllACBrLibNFe = path.join(__dirname, os.platform() === 'win32' ? 'ACBrNFe64.dll' : 'libacbrnfe64.so');
var pathXML = path.join(__dirname, 'chave-nfe.xml');

var eArqConfig = path.join(__dirname, 'acbrlib.ini');
var eChaveCrypt = '';
const acbrnfe = koffi.load(pathDllACBrLibNFe);
const libm =  { 
  NFE_Inicializar: acbrnfe.func('NFE_Inicializar', 'int', ['string', 'string']),
NFE_Finalizar: acbrnfe.func('NFE_Finalizar', 'int', []),
NFE_UltimoRetorno: acbrnfe.func('NFE_UltimoRetorno', 'int', ['char*', 'int*']),
NFE_Nome: acbrnfe.func('NFE_Nome', 'int', ['char*', 'int*']),
NFE_Versao: acbrnfe.func('NFE_Versao', 'int', ['char*', 'int*']),
NFE_OpenSSLInfo: acbrnfe.func('NFE_OpenSSLInfo', 'int', ['char *', 'int *']),

NFE_ConfigLer: acbrnfe.func('NFE_ConfigLer', 'int', ['string']),
NFE_ConfigGravar: acbrnfe.func('NFE_ConfigGravar', 'int', ['string']),
NFE_ConfigLerValor: acbrnfe.func('NFE_ConfigLerValor', 'int', ['string', 'string', 'char*', 'int*']),
NFE_ConfigGravarValor: acbrnfe.func('NFE_ConfigGravarValor', 'int', ['string', 'string', 'string']),
NFE_ConfigImportar: acbrnfe.func('NFE_ConfigImportar', 'int', ['string']),
NFE_ConfigExportar: acbrnfe.func('NFE_ConfigExportar', 'int', ['char*', 'int*']),

NFE_CarregarXML: acbrnfe.func('NFE_CarregarXML', 'int', ['string']),
NFE_CarregarINI: acbrnfe.func('NFE_CarregarINI', 'int', ['string']),
NFE_ObterXml: acbrnfe.func('NFE_ObterXml', 'int', ['int', 'char*', 'int*']),
NFE_GravarXml: acbrnfe.func('NFE_GravarXml', 'int', ['int', 'string', 'string']),
NFE_ObterIni: acbrnfe.func('NFE_ObterIni', 'int', ['int', 'char*', 'int*']),
NFE_GravarIni: acbrnfe.func('NFE_GravarIni', 'int', ['int', 'string', 'string']),
NFE_CarregarEventoXML: acbrnfe.func('NFE_CarregarEventoXML', 'int', ['string']),
NFE_CarregarEventoINI: acbrnfe.func('NFE_CarregarEventoINI', 'int', ['string']),
NFE_LimparLista: acbrnfe.func('NFE_LimparLista', 'int', []),
NFE_LimparListaEventos: acbrnfe.func('NFE_LimparListaEventos', 'int', []),
NFE_Assinar: acbrnfe.func('NFE_Assinar', 'int', []),
NFE_Validar: acbrnfe.func('NFE_Validar', 'int', []),
NFE_ValidarRegrasdeNegocios: acbrnfe.func('NFE_ValidarRegrasdeNegocios', 'int', ['char*', 'int*']),
NFE_VerificarAssinatura: acbrnfe.func('NFE_VerificarAssinatura', 'int', ['char*', 'int*']),
NFE_GerarChave: acbrnfe.func('NFE_GerarChave', 'int', ['int', 'int', 'int', 'int', 'int', 'int', 'string', 'string', 'char*', 'int*']),
NFE_ObterCertificados: acbrnfe.func('NFE_ObterCertificados', 'int', ['char*', 'int*']),
NFE_GetPath: acbrnfe.func('NFE_GetPath', 'int', ['int', 'char*', 'int*']),
NFE_GetPathEvento: acbrnfe.func('NFE_GetPathEvento', 'int', ['string', 'char*', 'int*']),

NFE_StatusServico: acbrnfe.func('NFE_StatusServico', 'int', ['char*', 'int*']),
NFE_Consultar: acbrnfe.func('NFE_Consultar', 'int', ['string', 'bool', 'char*', 'int*']),
NFE_ConsultarRecibo: acbrnfe.func('NFE_ConsultarRecibo', 'int', ['string', 'char*', 'int*']),
NFE_ConsultaCadastro: acbrnfe.func('NFE_ConsultaCadastro', 'int', ['string', 'string', 'bool', 'char*', 'int*']),

NFE_Inutilizar: acbrnfe.func('NFE_Inutilizar', 'int', ['string', 'string', 'int', 'int', 'int', 'int', 'int', 'char*', 'int*']),
NFE_Enviar: acbrnfe.func('NFE_Enviar', 'int', ['int', 'bool', 'bool', 'bool', 'char*', 'int*']),
NFE_Cancelar: acbrnfe.func('NFE_Cancelar', 'int', ['string', 'string', 'string', 'int', 'char*', 'int*']),
NFE_EnviarEvento: acbrnfe.func('NFE_EnviarEvento', 'int', ['int', 'char*', 'int*']),

NFE_DistribuicaoDFePorUltNSU: acbrnfe.func('NFE_DistribuicaoDFePorUltNSU', 'int', ['string', 'string', 'string', 'char*', 'int*']),
NFE_DistribuicaoDFePorNSU: acbrnfe.func('NFE_DistribuicaoDFePorNSU', 'int', ['int', 'string', 'string', 'char*', 'int*']),
NFE_DistribuicaoDFePorChave: acbrnfe.func('NFE_DistribuicaoDFePorChave', 'int', ['int', 'string', 'string', 'char*', 'int*']),
NFE_EnviarEmail: acbrnfe.func('NFE_EnviarEmail', 'int', ['string', 'string', 'bool', 'string', 'string', 'string', 'string']),
NFE_EnviarEmailEvento: acbrnfe.func('NFE_EnviarEmailEvento', 'int', ['string', 'string', 'string', 'bool', 'string', 'string', 'string', 'string']),

NFE_ImprimirPDF: acbrnfe.func('NFE_ImprimirPDF', 'int', []),
NFE_Imprimir: acbrnfe.func('NFE_Imprimir', 'int', ['string', 'int', 'string', 'string', 'string', 'string', 'string']),
NFE_SalvarPDF: acbrnfe.func('NFE_SalvarPDF', 'int', ['char*', 'int*']),
NFE_ImprimirEvento: acbrnfe.func('NFE_ImprimirEvento', 'int', ['string', 'string']),
NFE_ImprimirEventoPDF: acbrnfe.func('NFE_ImprimirEventoPDF', 'int', ['string', 'string']),
NFE_SalvarEventoPDF: acbrnfe.func('NFE_SalvarEventoPDF', 'int', ['string', 'string']),
NFE_ImprimirInutilizacao: acbrnfe.func('NFE_ImprimirInutilizacao', 'int', ['string']),
NFE_ImprimirInutilizacaoPDF: acbrnfe.func('NFE_ImprimirInutilizacaoPDF', 'int', ['string']),
NFE_SalvarInutilizacaoPDF: acbrnfe.func('NFE_SalvarInutilizacaoPDF', 'int', ['string'])
}

function getNFe() {



    let inicio = 2;

    let aloc_sResposta = Buffer.alloc(TAMANHO_BUFFER);

    //aloca ponteiro para o tamanho do buffer
    let aloc_esTamanho = koffi.alloc('int', 1);
    // configura o tamanho do buffer

    koffi.encode(aloc_esTamanho,"int",TAMANHO_BUFFER)

   inicio = libm.NFE_Inicializar(eArqConfig, eChaveCrypt);
    console.log(`iniciou >>>>>>> ${inicio}`);

    inicio = libm.NFE_ConfigGravarValor('DFe', 'ArquivoPFX', path.join(path.resolve(__dirname, 'certificado'), 'nome_do_certificado.pfx'));
    inicio = libm.NFE_ConfigGravarValor('DFe', 'Senha', '1234');
    inicio = libm.NFE_ConfigGravarValor('NFe', 'PathSchemas', path.resolve(__dirname, 'Schemas', 'NFe'));
    inicio  = libm.NFE_ConfigGravarValor('Principal','LogPath',__dirname)
    inicio = libm.NFE_ConfigGravarValor('Principal','LogNivel','4')

    libm.NFE_ConfigGravar(eArqConfig)
    
    console.log(`Set Configurações ${inicio}`);

    inicio = libm.NFE_CarregarXML(pathXML);
    console.log(`carregar xml >>>>>>> ${inicio}`);

    inicio = libm.NFE_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
    console.log(`ultmio retorno >>>>>>>> ${inicio}`);
    console.log(`Mensagem Ultimo Retorno: `, aloc_sResposta.toString());

    inicio = libm.NFE_Assinar();
    console.log(`assinar xml >>>>>>> ${inicio}`);

    aloc_sResposta = Buffer.alloc(TAMANHO_BUFFER);
    aloc_esTamanho = koffi.alloc('int', 1);
    koffi.encode(aloc_esTamanho, 'int', TAMANHO_BUFFER)

    inicio = libm.NFE_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
    console.log(`ultimo retorno apos assinar >>>>>>>> ${inicio}`);
    console.log(`Mensagem: `, aloc_sResposta.toString('ascii'));

    inicio = libm.NFE_Validar();
    console.log(`validar xml >>>>>>> ${inicio}`);


    aloc_sResposta = Buffer.alloc(TAMANHO_BUFFER);
    aloc_esTamanho = koffi.alloc("int",1)
    koffi.encode(aloc_esTamanho, 'int', TAMANHO_BUFFER)

    inicio = libm.NFE_UltimoRetorno(aloc_sResposta, aloc_esTamanho);
    console.log(`ultmio retorno apos validar >>>>>>>> ${inicio}`);
    console.log(`Mensagem: `, aloc_sResposta.toString());

    inicio = libm.NFE_Finalizar();
    console.log(`finalizar >>>>>>>> ${inicio}`);

    //recupera o tamanho do buffer
    let tamanho = koffi.decode(aloc_esTamanho, 'int');
    console.log(`tamanho do buffer: ${tamanho}`);
  }

getNFe();