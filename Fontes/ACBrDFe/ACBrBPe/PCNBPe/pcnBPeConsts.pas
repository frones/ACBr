////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar BPe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml do BPe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnBPeConsts;

interface

uses
  SysUtils;

const
  DSC_CHBPE = 'Chave do Bilhete de Passagem Eletrônico';
  DSC_INFQRCODEBPE = 'QR-Code do BP-e';
  DSC_BOARDPASSBPE = 'Boarding Pass do BP-e';
  DSC_MODALBPE = 'Modal do BP-e';
  DSC_TPBPE = 'Tipo de BP-e';
  DSC_INDPRESBPE = 'Indicador de Presença';
  DSC_UFINIBPE = 'UF de Inicio';
  DSC_CMUNINIBPE = 'Código do Municipio de Inicio';
  DSC_UFFIMBPE = 'UF de Fim';
  DSC_CMUNFIMBPE = 'Código do Municipio de Fim';
  DSC_CRTBPE = 'Código do Regime Tributário';
  DSC_TAR = 'Termo de Autorização de Serviço Regular';
  DSC_IDESTRBPE = 'Indicador de Comprador Estrangeiro';
  DSC_TPSUB = 'Tipo de Substituição';
  DSC_CLOCORIG = 'Código do Local de Origem';
  DSC_XLOCORIG = 'Descrição do Local de Origem';
  DSC_CLOCDEST = 'Código do Local de Destino';
  DSC_XLOCDEST = 'Descrição do Local de Destino';
  DSC_DHEMB = 'Data e Hora de Embarque';
  DSC_DHVALIDADE = 'Data e Hora de Validade';
  DSC_XNOMEPASS = 'Nome do Passageiro';
  DSC_TPDOC = 'Tipo de Documento';
  DSC_NDOC = 'Numero do Documento';
  DSC_DNASC = 'Data de Nascimento';
  DSC_CPERCURSO = 'Código do Percurso';
  DSC_XPERCURSO = 'Descrição do Percurso';
  DSC_TPVIAGEM = 'Tipo de Viagem';
  DSC_TPSERVICO = 'Tipo de Serviço';
  DSC_TPACOMODACAO = 'Tipo de Acomodação';
  DSC_TPTRECHO = 'Tipo de Trecho';
  DSC_DHVIAGEM = 'Data e Hora da Viagem';
  DSC_DHCONEXAO = 'Data e Hora da Conexão';
  DSC_INFVIAGEM = 'Informações da Viagem';
  DSC_PREFIXO = 'Prefixo da Linha';
  DSC_POLTRONA = 'Numero da Poltrona / Assento / Cabine';
  DSC_PLATAFORMA = 'Plataforma / Carro / Barco de Embarque';
  DSC_TPVEICULO = 'Tipo de Veículo Transportado';
  DSC_SITVEICULO = 'Situação do Veículo Transportado';
  DSC_VBP = 'Valor do Bilhete de Passagem';
  DSC_VDESCONTO = 'Valor do Desconto';
  DSC_VPGTO = 'Valor Pago';
  DSC_VTROCO = 'Valor do Troco';
  DSC_TPDESCONTO = 'Tipo de Desconto / Beneficio';
  DSC_XDESCONTO = 'Descrição do Tipo de Desconto / Beneficio Concedido';
  DSC_TPCOMP = 'Tipo de Componente';
  DSC_VCOMP = 'Valor do Componente';
  DSC_COMP = 'Componente do Valor do Bilhete';
  DSC_VCRED = 'Valor do Crédito Outorgado / Presumido';
  DSC_VTOTTRIB = 'Valor Aproximado dos Tributos';
  DSC_VBCUFFIM = 'Valor da BC';
  DSC_PFCPUFFIM = 'Percentual para Fundo ao Combate a Pobreza';
  DSC_PICMSUFFIM = 'Percentual do ICMS';
  DSC_PICMSINTER = 'Percentual do ICMS Interno';
  DSC_PICMSINTERPART = 'Percentual do ICMS Interno Parte';
  DSC_VFCPUFFIM = 'Valor do Fundo ao Combate a Pobreza';
  DSC_VICMSUFFIM = 'Valor do ICMS da UF de Fim';
  DSC_VICMSUFINI = 'Valor do ICMS da UF de Inicio';
  DSC_XXXX = 'XXXX';

implementation

end.

