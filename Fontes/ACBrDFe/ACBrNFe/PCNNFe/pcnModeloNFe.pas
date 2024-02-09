////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
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

{$I ACBr.inc}

unit pcnModeloNFe;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnConversao,
  pcnConversaoNFe,
  pcnNFe, pcnNFeW;

procedure ModeloNFe;

implementation

procedure ModeloNFe;
var
  NFe: TNFe;
  NFeW: TNFeW;
  i, j, k: integer;
  s: string;
  ReferenciadaTipoNFe: boolean;
  Opcao1: boolean;
begin

  // IMPORTANTE: Os laços For - Next codificados nesses modelo são meramente descritivos.
  //             Esse arquivo é apenas um modelo e deve ser adaptado conforme as suas necessidades.

  Opcao1 := True;      // Esta variavel esta sendo usada nesse modelo para indicar os locais onde
                       // devem ser tomadas decissões por parte do programador conforme a regras
                       // de negocio de cada cliente.

  NFe := TNFe.create;

  s := NFe.infNFe.ID;  // ATENÇÃO: Esse campo representa a chave da NFe
                       //          Não utilize esse campo para escrita (apenas para leitura)
                       //          pois a chave é gerada automaticamente no momento da geração do arquivo

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo das informações de identificação da NF-e  - <ide> - Ocorrência 1-1                                   *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

  NFe.Ide.cUF := 0;                     // B02 - Código da UF do emitente do Documento Fiscal - Tabela do IBGE
                                        //         Você pode utilizar a função UFparaCodigo caso não sáiba o código da UF
                                        //         ex: NFe.Ide.cUF := UFparaCodigo(MinhaUF);
  NFe.Ide.cNF := -1;                    // B03 - Código Numérico que compõe a Chave de Acesso
                                        //         Se nenhum valor for informado será atribuido um valor aleatório
                                        //         Se for informado o valor -1; será gerado um valor baseado no numero da NFe
  NFe.Ide.natOp := '';                  // B04 - Descrição da Natureza da Operação
  NFe.Ide.indPag := ipVista;            // B05 - Indicador da forma de pagamento (*)
                                        //         (0)=ipVista
                                        //         (1)=ipPrazo
                                        //         (2)=ipOutras
  NFe.Ide.modelo := 55;                 // B06 - Código do Modelo do Documento Fiscal Utilizar o código 55 para identificação da NF-e, emitida em substituição ao modelo 1 ou 1A.
  NFe.Ide.serie := 0;                   // B07 - Série do Documento Fiscal, informar 0 (zero) para série única.
  NFe.Ide.nNF := 0;                     // B08 - Número do Documento Fiscal
  NFe.Ide.dEmi := null;                 // B09 - Data de emissão do Documento Fiscal
  NFe.Ide.dSaiEnt := null;              // B10 - Data de Saída ou da Entrada da Mercadoria/Produto
  NFe.Ide.tpNF := tnSaida;              // B11 - Tipo do Documento Fiscal (*)
                                        //         (0)=tnEntrada
                                        //         (1)=tnSaida
  NFe.Ide.cMunFG := 0;                  // B12 - Código do Município de Ocorrência do Fato Gerador do ICMS ( Tab. IBGE )
  NFe.Ide.tpImp := tiRetrato;           // B21 - Formato de Impressão do DANFE (*)
                                        //         (1)=tiRetrato
                                        //         (2)=tiPaisagem
  NFe.Ide.tpEmis := teNormal;           // B22 - Forma de Emissão da NF-e (*)
                                        //         (1)=teNormal
                                        //         (2)=teContingencia
                                        //         (3)=teSCAN
                                        //         (4)=teDPEC
                                        //         (5)=teFSDA
  // Ex: i := NFe.Ide.cDv;              // B23 - Dígito Verificador da Chave de  Acesso da NF-e
                                        //         Não utilize esse campo para escrita (apenas para leitura)
                                        //         pois o digito é gerado automaticamente no momento da geração do arquivo
  NFe.Ide.tpAmb := taProducao;          // B24 - Identificação do Ambiente (*)
                                        //         (1)=Produção
                                        //         (2)=Homologação
  NFe.Ide.finNFe := fnNormal;           // B25 - Finalidade de emissão da NF-e (*)
                                        //         (1)=fnNormal
                                        //         (2)=fnComplementar
                                        //         (3)=fnAjuste
  NFe.Ide.procEmi :=                    // B26 - Processo de emissão da NF-e (*)
    peAplicativoContribuinte;           //         (0)=peAplicativoContribuinte      - emissão de NF-e com aplicativo do contribuinte;
                                        //         (1)=peAvulsaFisco                 - emissão de NF-e avulsa pelo Fisco;
                                        //         (2)=peAvulsaContribuinte          - emissão de NF-e avulsa, pelo contribuinte com seu certificado digital, através do site do Fisco;
                                        //         (3)=peContribuinteAplicativoFisco - emissão NF-e pelo contribuinte com aplicativo fornecido pelo Fisco.
  NFe.Ide.verProc := '';                // B27 - Versão do Processo de emissão da NF-e

  ReferenciadaTipoNFe := True;          // TAG - Informação das NF/NF-e referenciadas - <NFref> - Ocorrência 0-N ********
  if ReferenciadaTipoNFe then
  begin                                 // Se a nota referenciada for um NFe preencher o campo abaixo:
    NFe.Ide.NFref.Add;
    NFe.Ide.NFref[0].refNFe := '';      // B13 - Chave de acesso das NF-e referenciadas
  end;
  if not ReferenciadaTipoNFe then
  begin                                 // Se a nota referenciada não for uma NFe preencher o campos abaixo:
    NFe.Ide.NFref.Add;
    NFe.Ide.NFref[1].RefNF.cUF := 0;    // B15 - Código da UF do emitente do Documento Fiscal - Tabela do IBGE
    NFe.Ide.NFref[1].RefNF.AAMM := '';  // B16 - Ano e Mês de emissão da NF-e
    NFe.Ide.NFref[1].RefNF.CNPJ := '';  // B17 - CNPJ do emitente
    NFe.Ide.NFref[1].RefNF.modelo := 0; // B18 - Modelo do Documento Fiscal - Ex. 01
    NFe.Ide.NFref[1].RefNF.serie := 0;  // B19 - Série do Documento Fiscal - informar zero se inexistente
    NFe.Ide.NFref[1].RefNF.nNF := 0;    // B20 - Número do Documento Fiscal
  end;

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do emitente da NF-e  - <emit> - Ocorrência 1-1                                      *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

  NFe.Emit.CNPJCPF := '';           // C02 - CNPJ do emitente / CPF do remetente
  NFe.Emit.xNome := '';             // C03 - Razão Social ou Nome do emitente
  NFe.Emit.xFant := '';             // C04 - Nome fantasia
  NFe.Emit.IE := '';                // C17 - Inscrição Estadual do emitente
  NFe.Emit.IEST := '';              // C18 - Inscrição Estadual do Substituto Tributário da UF de destino da mercadoria, quando houver a retenção do ICMS ST para a UF de destino.
  NFe.Emit.IM := '';                // C19 - Inscrição Municipal (NF-e conjugada, com prestação de serviços sujeitos ao ISSQN e fornecimento de peças sujeitos ao ICMS.)
  NFe.Emit.CNAE := '';              // C20 - CNAE fiscal Este campo deve ser informado quando o campo NFe.Emit.IM for informado.
                                    //       TAG de grupo do Endereço do emitente - <enderEmit> - Ocorrência 1-1 ********

// Henrique Leonardo
  Nfe.Emit.CRT  := crtRegimeNormal; // C21  - Código de Regime Tributário
                                    //  Este campo será obrigatoriamente preenchido com:
                                    //  crtSimplesNacional        1 – Simples Nacional;
                                    //  crtSimplesExcessoReceita  2 – Simples Nacional – excesso de sublimite de receita bruta;
                                    //  crtRegimeNormal           3 – Regime Normal. (v2.0).

// Henrique Leonardo

  NFe.Emit.enderEmit.xLgr     := '';    // C06 - Logradouro
  NFe.Emit.enderEmit.nro      := '';    // C07 - Número
  NFe.Emit.enderEmit.xCpl     := '';    // C08 - Complemento
  NFe.Emit.enderEmit.xBairro  := '';    // C09 - Bairro
  NFe.Emit.enderEmit.cMun     := 0;     // C10 - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  NFe.Emit.enderEmit.xMun     := '';    // C11 - Nome do município   ('EXTERIOR' para operações com o exterior)
  NFe.Emit.enderEmit.UF       := '';    // C12 - Sigla da UF         ('EX' para operações com o exterior.)
  NFe.Emit.enderEmit.CEP      := 0;     // C13 - Código do CEP
  NFe.Emit.enderEmit.cPais    := 0;     // C14 - Código do País      (Tabela do BACEN )
  NFe.Emit.enderEmit.xPais    := '';    // C15 - Nome do País
  NFe.Emit.enderEmit.fone     := '';    // C16 - Telefone            ( Código DDD + número do telefone. )


  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de Identificação do Fisco Emitente da NF-e - <avulsa> - Ocorrência 0-1                               *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                                    // D01 - Grupo para uso exclusivo do fisco
  NFe.Avulsa.CNPJ := '';
  NFe.Avulsa.xOrgao := '';
  NFe.Avulsa.matr := '';
  NFe.Avulsa.xAgente := '';
  NFe.Avulsa.fone := '';
  NFe.Avulsa.UF := '';
  NFe.Avulsa.nDAR := '';
  NFe.Avulsa.dEmi := null;
  NFe.Avulsa.vDAR := 0;
  NFe.Avulsa.repEmi := '';
  NFe.Avulsa.dPag := null;

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do Destinatário da NF-e  - <dest> - Ocorrência 1-1                                  *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

  NFe.Dest.CNPJCPF := '';           // E02 - CNPJ do destinatário / CPF do destinatário
  NFe.Dest.xNome := '';             // E04 - Razão Social ou nome do destinatário
  NFe.Dest.IE := '';                // E17 - Inscrição Estadual do destinatário
  NFe.Dest.ISUF := '';              // E18 - Inscrição na SUFRAMA
                                    //       TAG de grupo de endereço do Destinatário da NF-e - <enderDest> - Ocorrência 1-1 *
  NFe.Dest.enderDest.xLgr := '';    // E06 - Logradouro
  NFe.Dest.enderDest.nro := '';     // E07 - Número
  NFe.Dest.enderDest.xCpl := '';    // E08 - Complemento
  NFe.Dest.enderDest.xBairro := ''; // E09 - Bairro
  NFe.Dest.enderDest.cMun := 0;     // E10 - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  NFe.Dest.enderDest.xMun := '';    // E11 - Nome do município   ('EXTERIOR' para operações com o exterior)
  NFe.Dest.enderDest.UF := '';      // E12 - Sigla da UF         ('EX' para operações com o exterior.)
  NFe.Dest.enderDest.CEP := 0;      // E13 - Código do CEP
  NFe.Dest.enderDest.cPais := 0;    // E14 - Código do País      (Tabela do BACEN )
  NFe.Dest.enderDest.xPais := '';   // E15 - Nome do País
  NFe.Dest.enderDest.fone := '';    // E16 - Telefone            ( Código DDD + número do telefone. )

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do Local de retirada - <retirada> - Ocorrência 0-N                                  *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                              // Informar os valores desse grupo somente se o  endereço de
                              // retirada for diferente do endereço do remetente.
                              // Assim se retirada.xLgr <> '' o gerador grava o grupo no XML

  NFe.Retirada.CNPJCPF := '';    // F02 - CNPJ
  NFe.Retirada.xLgr := '';    // F03 - Logradouro
  NFe.Retirada.nro := '';     // F04 - Número
  NFe.Retirada.xCpl := '';    // F05 - Complemento
  NFe.Retirada.xBairro := ''; // F06 - Bairro
  NFe.Retirada.cMun := 0;     // F07 - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  NFe.Retirada.xMun := '';    // F08 - Nome do município   ('EXTERIOR' para operações com o exterior)
  NFe.Retirada.UF := '';      // F09 - Sigla da UF         ('EX' para operações com o exterior.)

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do Local de entrega - <entrega> - Ocorrência 0-N                                    *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                              // Informar os valores desse grupo somente se o
                              // endereço de entrega for diferente do endereço do destinatario.
                              // Assim se entrega.xLgr <> '' o gerador grava o grupo no XML

  NFe.Entrega.CNPJCPF := '';     // G02 - CNPJ
  NFe.Entrega.xLgr := '';     // G03 - Logradouro
  NFe.Entrega.nro := '';      // G04 - Número
  NFe.Entrega.xCpl := '';     // G05 - Complemento
  NFe.Entrega.xBairro := '';  // G06 - Bairro
  NFe.Entrega.cMun := 0;      // G07 - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  NFe.Entrega.xMun := '';     // G08 - Nome do município   ('EXTERIOR' para operações com o exterior)
  NFe.Entrega.UF := '';       // G09 - Sigla da UF         ('EX' para operações com o exterior.)

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo do detalhamento de Produtos e Serviços da NF-e - <det> - Ocorrência 1-990                            *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

  for i := 0 to 1 do
  begin

    NFe.Det.Add;

    NFe.Det[i].infAdProd := '';     // - Informações Adicionais do Produto

    NFe.Det[i].Prod.nItem := 1 + i; // H02 - Número do item DE 1 a 990
    NFe.Det[i].Prod.cProd := '';    // I02 - Código do produto ou serviço
    NFe.Det[i].Prod.cEAN := '';     // I03 - GTIN (Global Trade Item Number) do produto, antigo código EAN ou código de barra
    NFe.Det[i].Prod.xProd := '';    // I04 - Descrição do produto ou serviço
    NFe.Det[i].Prod.NCM := '';      // I05 - Código NCM         (Em caso de serviço, não incluir a TAG.)
    NFe.Det[i].Prod.EXTIPI := '';   // I06 - Código EX da TIPI  (Em caso de serviço, não incluir a TAG.)
//    NFe.Det[i].Prod.genero := 0;    // I07 - Gênero do Produto ou Serviço (Tabela de Capítulos da NCM)(Em caso de serviço, não incluir a TAG.)
    NFe.Det[i].Prod.CFOP := '';     // I08 - Código Fiscal de Operações e Prestações

    NFe.Det[i].Prod.uCom := '';     // I09 - Unidade Comercial
    NFe.Det[i].Prod.qCom := 0;      // I10 - Quantidade Comercial
    NFe.Det[i].Prod.vUnCom := 0;    // I10a  Valor Unitário de comercialização
    NFe.Det[i].Prod.vProd := 0;     // I11 - Valor Total Bruto dos Produtos ou Serviços
    NFe.Det[i].Prod.uTrib := '';    // I13 - Unidade Tributável
    NFe.Det[i].Prod.qTrib := 0;     // I14 - Quantidade Tributável
    NFe.Det[i].Prod.vUnTrib := 0;   // I14a  Valor Unitário de tributação

    (* EXEMPLO *)                   // NFe.Det[i].Prod.uCom    := 'CX';
                                    // NFe.Det[i].Prod.qCom    :=    2;   Vendidas 2 caixas ( com 10 unidades cada )
                                    // NFe.Det[i].Prod.vUnCom  :=   50;   R$ 50,00 cada caixa
                                    // NFe.Det[i].Prod.vProd   :=  100;   R$ 100,00 Valor dos produtos
                                    // NFe.Det[i].Prod.uTrib   := 'UN';
                                    // NFe.Det[i].Prod.qTrib   :=   20;   2 caixas X 10 unidades por caixa = 20 unidades
                                    // NFe.Det[i].Prod.vUnTrib :=    5;   R$ 100,00 / 20 unidades = R$ 5,00 cada unidade

    NFe.Det[i].Prod.cEANTrib := ''; // I12 - GTIN (Global Trade Item Number) da unidade tributável,  antigo código EAN ou código de  barras ??
    NFe.Det[i].Prod.vFrete := 0;    // I15 - Valor Total do Frete
    NFe.Det[i].Prod.vSeg := 0;      // I16 - Valor Total do Seguro
    NFe.Det[i].Prod.vDesc := 0;     // I17 - Valor do Desconto

    //                                                           Tag da Declaração de Importação - <DI> - Ocorrência 0-N

    for j := 0 to 1 do
    begin
      NFe.Det[i].Prod.DI.Add;
      NFe.Det[i].Prod.DI[j].nDi := '';                  // I19 - Número do Documento de Importação DI/DSI/DA (DI/DSI/DA)
      NFe.Det[i].Prod.DI[j].dDi := null;                // I20 - Data de Registro da DI/DSI/DA
      NFe.Det[i].Prod.DI[j].xLocDesemb := '';           // I21 - Local de desembaraço
      NFe.Det[i].Prod.DI[j].UFDesemb := '';             // I22 - Sigla da UF onde ocorreu o Desembaraço Aduaneiro
      NFe.Det[i].Prod.DI[j].dDesemb := null;            // I23 - Data do Desembaraço Aduaneiro
      NFe.Det[i].Prod.DI[j].cExportador := '';          // I24 - Código do exportador
      for k := 0 to 1 do
      begin
        NFe.Det[i].Prod.DI[j].adi.Add;                  //       Tag de Adições - <adi> - Ocorrência 1-N
        NFe.Det[i].Prod.DI[j].adi[k].nAdicao := 0;      // I26 - Numero da adição
        NFe.Det[i].Prod.DI[j].adi[k].nSeqAdi := 0;      // I27 - Numero seqüencial do item dentro da adição
        NFe.Det[i].Prod.DI[j].adi[k].cFabricante := ''; // I28 - Código do fabricante estrangeiro
        NFe.Det[i].Prod.DI[j].adi[k].vDescDI := 0;      // I29 - Valor do desconto do item da DI – adição
      end;
    end;

    //                                                      TAG de grupo do detalhamento de Veículos novos - <veicProd> - Ocorrência 0-1

                                                    //       Este grupo será criado no XML somente se o campo <chassi> for informado.
    NFe.Det[i].prod.veicProd.tpOP := toVendaDireta; // J02 - Tipo da operação
                                                    //         (1)=toVendaConcessionaria
                                                    //         (2)=toFaturamentoDireto
                                                    //         (3)=toVendaDireta
                                                    //         (0)=toOutros
    NFe.Det[i].prod.veicProd.chassi := '';          // J03 - Chassi do veículo
    NFe.Det[i].prod.veicProd.cCor := '';            // J04 - Cor
    NFe.Det[i].prod.veicProd.xCor := '';            // J05 - Descrição da Cor
    NFe.Det[i].prod.veicProd.pot := '';             // J06 - Potência Motor
//    NFe.Det[i].prod.veicProd.CM3 := '';             // J07 - CM3 (Potência)
    NFe.Det[i].prod.veicProd.pesoL := '';           // J08 - Peso Líquido
    NFe.Det[i].prod.veicProd.pesoB := '';           // J09 - Peso Bruto
    NFe.Det[i].prod.veicProd.nSerie := '';          // J10 - Serial (série)
    NFe.Det[i].prod.veicProd.tpComb := '';          // J11 - Tipo de combustível
    NFe.Det[i].prod.veicProd.nMotor := '';          // J12 - Número de Motor
//    NFe.Det[i].prod.veicProd.CMKG := '';            // J13 - CMKG
    NFe.Det[i].prod.veicProd.dist := '';            // J14 - Distância entre eixos
//    NFe.Det[i].prod.veicProd.RENAVAM := '';         // J15 - RENAVAM            (Não informar a TAG na exportação)
    NFe.Det[i].prod.veicProd.anoMod := 0;           // J16 - Ano Modelo de Fabricação
    NFe.Det[i].prod.veicProd.anoFab := 0;           // J17 - Ano de Fabricação
    NFe.Det[i].prod.veicProd.tpPint := '';          // J18 - Tipo de Pintura
    NFe.Det[i].prod.veicProd.tpVeic := 0;           // J19 - Tipo de Veículo    (Utilizar Tabela RENAVAM)
    NFe.Det[i].prod.veicProd.espVeic := 0;          // J20 - Espécie de Veículo (Utilizar Tabela RENAVAM)
    NFe.Det[i].prod.veicProd.VIN := '';             // J21 - Condição do VIN
    NFe.Det[i].prod.veicProd.condVeic := cvAcabado; // J22 - Condição do Veículo (1-Acabado; 2-Inacabado; 3-Semi-acabado)
    NFe.Det[i].prod.veicProd.cMod := '';             // J23 - Código Marca Modelo (Utilizar Tabela RENAVAM)

    //                                              TAG de grupo do detalhamento de Medicamentos - <med> - Ocorrência 0-N
    for j := 0 to 1 do
    begin
      NFe.Det[i].prod.med.add;
      NFe.Det[i].prod.med[j].nLote := '';  // K02 - Número do Lote do medicamento
      NFe.Det[i].prod.med[j].qLote := 0;   // K03 - Quantidade de produto no Lote do medicamento
      NFe.Det[i].prod.med[j].dFab := null; // K04 - Data de fabricação
      NFe.Det[i].prod.med[j].dVal := null; // K05 - Data de validade
      NFe.Det[i].prod.med[j].vPMC := 0;    // K06 - Preço máximo consumidor
    end;

    //                                                           TAG de grupo do detalhamento de Armamento - <arma> - Ocorrência 0-N
    for j := 0 to 1 do
    begin
      NFe.Det[i].prod.arma.add;
      NFe.Det[i].prod.arma[j].tpArma := taUsoPermitido; // L02 - Indicador do tipo de arma de fogo
                                                        //         (0)=taUsoPermitido
                                                        //         (1)=taUsoRestrito
      NFe.Det[i].prod.arma[j].nSerie := '0';              // L03 - Número de série da arma
      NFe.Det[i].prod.arma[j].nCano := '0';               // L04 - Número de série do cano
      NFe.Det[i].prod.arma[j].descr := '';              // L05 - Descrição completa da arma, compreendendo: calibre, marca, capacidade, etc)
    end;

    //                                                            TAG de grupo de informações específicas para combustíveis

                                                        //        líquidos - <comb> - Ocorrência 0-1
                                                        //        Se for informado algum dos valores abaixo:
                                                        //        a TAG grava o grupo no XML ********************************************
    NFe.Det[i].prod.comb.cProdANP := 0;                 // L102 - Código de produto da ANP - codificação de produtos do SIMP
    NFe.Det[i].prod.comb.CODIF    := '';                   // L103 - Código de autorização / registro do CODIF
    NFe.Det[i].prod.comb.qTemp    := 0;                    // L104 - Quantidade de combustível faturada à temperatura ambiente.
                                                        //        TAG de grupo da CIDE - <CIDE> - Ocorrência 0-1
    NFe.Det[i].prod.comb.CIDE.qBCprod := 0;             // L106 - BC da CIDE em quantidad
    NFe.Det[i].prod.comb.CIDE.vAliqProd := 0;           // L107 - Valor da alíquota da CIDE
    NFe.Det[i].prod.comb.CIDE.vCIDE := 0;               // L108 - Valor da CIDE
                                                        //        TAG de grupo do ICMS - <ICMS> - Ocorrência 1-1
    NFe.Det[i].prod.comb.ICMS.vBCICMS := 0;             // L110 - BC do ICMS
    NFe.Det[i].prod.comb.ICMS.vICMS := 0;               // L111 - Valor do ICMS
    NFe.Det[i].prod.comb.ICMS.vBCICMSST := 0;           // L112 - BC do ICMS ST retido
    NFe.Det[i].prod.comb.ICMS.vICMSST := 0;             // L113 - Valor do ICMS ST retido
                                                        //        TAG de grupo do ICMSST de operação interestadual - <ICMSInter> - Ocorrência 0-1
    NFe.Det[i].prod.comb.ICMSInter.vBCICMSSTDest := 0;  // L115 - BC do ICMS ST da UF de destino
    NFe.Det[i].prod.comb.ICMSInter.vICMSSTDest := 0;    // L116 - Valor do ICMS ST da UF de destino
                                                        //        TAG de ICMS para consumo em UF diversa da UF de localização do destinatário do produto - <ICMSCons> - Ocorrência 0-1
    NFe.Det[i].prod.comb.ICMSCons.vBCICMSSTCons := 0;   // L118 - BC do ICMS ST da UF de consumo
    NFe.Det[i].prod.comb.ICMSCons.vICMSSTCons := 0;     // L119 - Valor do ICMS ST da UF de consumo
    NFe.Det[i].prod.comb.ICMSCons.UFcons := '';         // L120 - Sigla da UF de consumo

    //                                                                      TAG de grupo do ICMS da Operação própria e ST - <ICMS> - Ocorrência 1-1

    NFe.Det[i].Imposto.ICMS.orig := oeNacional;                    // N11 - Origem da mercadoria
                                                                   //         (0)=oeNacional
                                                                   //         (1)=oeEstrangeiraImportacaoDireta
                                                                   //         (2)=oeEstrangeiraAdquiridaBrasil
    NFe.Det[i].Imposto.ICMS.CST := cst00;

    // Henrique Leonardo
    case nfe.Emit.CRT of
      crtRegimeNormal, crtSimplesExcessoReceita :
         begin
            case nfe.Det[i].Imposto.ICMS.CST of
               cst00 :
                  begin
                    NFe.Det[i].Imposto.ICMS.modBC := dbiMargemValorAgregado;     // N13 - Modalidade de determinação da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.vBC   := 0;                          // N15 - Valor da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pICMS := 0;                          // N16 - Alíquota do imposto
                    NFe.Det[i].Imposto.ICMS.vICMS := 0;                          // N17 - Valor do ICMS
                  end;
               cst10 :
				  begin
                    NFe.Det[i].Imposto.ICMS.modBC     := dbiMargemValorAgregado;    // N13 - Modalidade de determinação da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.vBC       := 0;                         // N15 - Valor da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pICMS     := 0;                         // N16 - Alíquota do imposto
                    NFe.Det[i].Imposto.ICMS.vICMS     := 0;                         // N17 - Valor do ICMS
                    NFe.Det[i].Imposto.ICMS.modBCST   := dbisMargemValorAgregado;   // N18 - Modalidade de determinação da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pMVAST    := 0;                         // N19 - Percentual da margem de valor Adicionado do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pRedBCST  := 0;                         // N20 - Percentual da Redução de BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vBCST     := 0;                         // N21 - Valor da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pICMSST   := 0;                         // N22 - Alíquota do imposto do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vICMSST   := 0;                         // N23 - Valor do ICMS ST
                  end;
               cst20 :
                  begin
                    NFe.Det[i].Imposto.ICMS.modBC   := dbiMargemValorAgregado;      // N13 - Modalidade de determinação da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pRedBC  := 0;                           // N14 - Percentual da Redução de BC do ICMS
                    NFe.Det[i].Imposto.ICMS.vBC     := 0;                           // N15 - Valor da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pICMS   := 0;                           // N16 - Alíquota do imposto
                    NFe.Det[i].Imposto.ICMS.vICMS   := 0;                           // N17 - Valor do ICMS
                  end;
               cst30 :
                  begin
                    NFe.Det[i].Imposto.ICMS.modBCST   := dbisMargemValorAgregado; // N18 - Modalidade de determinação da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pMVAST    := 0;                       // N19 - Percentual da margem de valor Adicionado do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pRedBCST  := 0;                       // N20 - Percentual da Redução de BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vBCST     := 0;                       // N21 - Valor da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pICMSST   := 0;                       // N22 - Alíquota do imposto do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vICMSST   := 0;                       // N23 - Valor do ICMS ST
                  end;
               cst40,
               cst41,
	             cst50 :
                  begin
                     //Esse bloco fica a critério de cada UF a obrigação das informações, conforme o manual
                    NFe.Det[i].Imposto.ICMS.vICMS       := 0;                     // N17 - Valor do ICMS
                    Nfe.Det[i].Imposto.ICMS.motDesICMS  := mdiOutros;             // N28 - Motivo da desoneração do ICMS
                  end;
               cst51 :
                  begin
                     //Esse bloco fica a critério de cada UF a obrigação das informações, conforme o manual

                     NFe.Det[i].Imposto.ICMS.modBC  := dbiMargemValorAgregado;    // N13 - Modalidade de determinação da BC do ICMS
                                                                                    //  0 - Margem Valor Agregado (%);
                                                                                    //  1 - Pauta (Valor);
                                                                                    //  2 - Preço Tabelado Máx. (valor);
                                                                                    //  3 - valor da operação

                     NFe.Det[i].Imposto.ICMS.pRedBC := 0;                          // N14 - Percentual da Redução de BC do ICMS
                     NFe.Det[i].Imposto.ICMS.vBC    := 0;                          // N15 - Valor da BC do ICMS
                     NFe.Det[i].Imposto.ICMS.pICMS  := 0;                          // N16 - Alíquota do imposto
                     NFe.Det[i].Imposto.ICMS.vICMS  := 0;                          // N17 - Valor do ICMS
                  end;
               cst60 :
                  begin
                     nfe.Det[i].Imposto.ICMS.vBCSTRET   := 0;                     // N26 - Valor da BC do ICMS ST retido
                     nfe.Det[i].Imposto.ICMS.vICMSSTRET := 0;                     // N27 - Valor do ICMS ST retido
                  end;
               cst70 :
                  begin
                    NFe.Det[i].Imposto.ICMS.modBC     := dbiMargemValorAgregado;  // N13 - Modalidade de determinação da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pRedBC    := 0;                       // N14 - Percentual da Redução de BC do ICMS
                    NFe.Det[i].Imposto.ICMS.vBC       := 0;                       // N15 - Valor da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pICMS     := 0;                       // N16 - Alíquota do imposto
                    NFe.Det[i].Imposto.ICMS.vICMS     := 0;                       // N17 - Valor do ICMS
                    NFe.Det[i].Imposto.ICMS.modBCST   := dbisMargemValorAgregado; // N18 - Modalidade de determinação da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pMVAST    := 0;                       // N19 - Percentual da margem de valor Adicionado do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pRedBCST  := 0;                       // N20 - Percentual da Redução de BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vBCST     := 0;                       // N21 - Valor da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pICMSST   := 0;                       // N22 - Alíquota do imposto do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vICMSST   := 0;                       // N23 - Valor do ICMS ST
                  end;
               cst90,
		           cstPart10,
	             cstPart90 :
                  begin
                    NFe.Det[i].Imposto.ICMS.modBC     := dbiMargemValorAgregado;    // N13 - Modalidade de determinação da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pRedBC    := 0;                         // N14 - Percentual da Redução de BC do ICMS
                    NFe.Det[i].Imposto.ICMS.vBC       := 0;                         // N15 - Valor da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pICMS     := 0;                         // N16 - Alíquota do imposto
                    NFe.Det[i].Imposto.ICMS.vICMS     := 0;                         // N17 - Valor do ICMS
                    NFe.Det[i].Imposto.ICMS.modBCST   := dbisMargemValorAgregado;   // N18 - Modalidade de determinação da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pMVAST    := 0;                         // N19 - Percentual da margem de valor Adicionado do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pRedBCST  := 0;                         // N20 - Percentual da Redução de BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vBCST     := 0;                         // N21 - Valor da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pICMSST   := 0;                         // N22 - Alíquota do imposto do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vICMSST   := 0;                         // N23 - Valor do ICMS ST
                    if ( nfe.Det[i].Imposto.ICMS.CST = cstPart10 ) or ( nfe.Det[i].Imposto.ICMS.CST = cstPart90 ) then
                    begin
                      NFe.Det[i].Imposto.ICMS.UFST  := '';                         // N24  UF para qual é devido o ICMS ST
                      NFe.Det[i].Imposto.ICMS.pBCOp := 0;                          // N25  Percentual da BC operação própria
                     end;
                  end;
	            	cstRep41 :
                  begin
                    // ICMSST - Repasse
                    NFe.Det[i].Imposto.ICMS.vBCSTRet    := 0;                     // N26 - Valor da BC do ICMS ST retido
                    NFe.Det[i].Imposto.ICMS.vICMSSTRet  := 0;                     // N27 - Valor do ICMS ST retido
                    NFe.Det[i].Imposto.ICMS.vBCSTDest   := 0;                     // N31 - Informar o valor da BC do ICMS ST da UF destino (v2.0)
                    NFe.Det[i].Imposto.ICMS.vICMSSTDest := 0;                     // N32 - Informar o valor da BC do ICMS ST da UF destino (v2.0)
		              end;
            end;
         end;
      crtSimplesNacional :
         begin
            //Grupo do Simples Nacional
            case  nfe.Det[i].Imposto.ICMS.CSOSN of
               csosn101 :  // N12a
                  begin
                    nfe.Det[i].Imposto.ICMS.pCredSN     := 0;                     // N29 - Alíquota aplicável de cálculo do crédito (Simples Nacional).
                    nfe.Det[i].Imposto.ICMS.vCredICMSSN := 0;                     // N30 - Valor crédito do ICMS que pode ser aproveitado nostermos do art. 23 da LC 123 (Simples Nacional)
                  end;
               csosn102,
               csosn103,
               csosn300,
               csosn400:  // N10d
                  begin
                     //Tags ORIG e CSON já criadas antes do case
                     //102 - Tributada pelo Simples Nacional sem permissão de crédito.
                     //103 – Isenção do ICMS no Simples Nacional para faixa de receita bruta.
                     //300 – Imune.
                     //400 – Não tributada pelo Simples Nacional (v.2.0) (v.2.0)
                  end;
               csosn201 :
                  begin  //n10e
                    nfe.Det[i].Imposto.ICMS.modBCST     := dbisMargemValorAgregado;   // N18 - Modalidade de determinação da BC do ICMS ST
                    nfe.Det[i].Imposto.ICMS.pMVAST      := 0;                         // N19 - Percentual da margem de valor Adicionado do ICMS ST
                    nfe.Det[i].Imposto.ICMS.pRedBCST    := 0;                         // N20 - Percentual da Redução de BC do ICMS ST
                    nfe.Det[i].Imposto.ICMS.vBCST       := 0;                         // N21 - Valor da BC do ICMS ST
                    nfe.Det[i].Imposto.ICMS.pICMSST     := 0;                         // N22 - Alíquota do imposto do ICMS ST
                    nfe.Det[i].Imposto.ICMS.vICMSST     := 0;                         // N23 - Valor do ICMS ST
                    nfe.Det[i].Imposto.ICMS.pCredSN     := 0;                         // N29 - Alíquota aplicável de cálculo do crédito (Simples Nacional).
                    nfe.Det[i].Imposto.ICMS.vCredICMSSN := 0;                         // N30 - Valor crédito do ICMS que pode ser aproveitado nostermos do art. 23 da LC 123 (Simples Nacional)
                  end;
               csosn202,
               csosn203 :
                  begin   //n10f
                    nfe.Det[i].Imposto.ICMS.modBCST     := dbisMargemValorAgregado;   // N18 - Modalidade de determinação da BC do ICMS ST
                    nfe.Det[i].Imposto.ICMS.pMVAST      := 0;                         // N19 - Percentual da margem de valor Adicionado do ICMS ST
                    nfe.Det[i].Imposto.ICMS.pRedBCST    := 0;                         // N20 - Percentual da Redução de BC do ICMS ST
                    nfe.Det[i].Imposto.ICMS.vBCST       := 0;                         // N21 - Valor da BC do ICMS ST
                    nfe.Det[i].Imposto.ICMS.pICMSST     := 0;                         // N22 - Alíquota do imposto do ICMS ST
                    nfe.Det[i].Imposto.ICMS.vICMSST     := 0;                         // N23 - Valor do ICMS ST
                  end;
               csosn500 :
                  begin //10g
                     nfe.Det[i].Imposto.ICMS.vBCSTRET   := 0;                     // N26 - Valor da BC do ICMS ST retido
                     nfe.Det[i].Imposto.ICMS.vICMSSTRET := 0;                     // N27 - Valor do ICMS ST retido
                  end;
               csosn900:
                 begin //10h
                    NFe.Det[i].Imposto.ICMS.modBC       := dbiMargemValorAgregado;    // N13 - Modalidade de determinação da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pRedBC      := 0;                         // N14 - Percentual da Redução de BC do ICMS
                    NFe.Det[i].Imposto.ICMS.vBC         := 0;                         // N15 - Valor da BC do ICMS
                    NFe.Det[i].Imposto.ICMS.pICMS       := 0;                         // N16 - Alíquota do imposto
                    NFe.Det[i].Imposto.ICMS.vICMS       := 0;                         // N17 - Valor do ICMS
                    NFe.Det[i].Imposto.ICMS.modBCST     := dbisMargemValorAgregado;   // N18 - Modalidade de determinação da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pMVAST      := 0;                         // N19 - Percentual da margem de valor Adicionado do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pRedBCST    := 0;                         // N20 - Percentual da Redução de BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vBCST       := 0;                         // N21 - Valor da BC do ICMS ST
                    NFe.Det[i].Imposto.ICMS.pICMSST     := 0;                         // N22 - Alíquota do imposto do ICMS ST
                    NFe.Det[i].Imposto.ICMS.vICMSST     := 0;                         // N23 - Valor do ICMS ST
                    nfe.Det[i].Imposto.ICMS.pCredSN     := 0;                         // N29 - Alíquota aplicável de cálculo do crédito (Simples Nacional).
                    nfe.Det[i].Imposto.ICMS.vCredICMSSN := 0;                         // N30 - Valor crédito do ICMS que pode ser aproveitado nostermos do art. 23 da LC 123 (Simples Nacional)
                 end;
            end;
         end;
	  end;
    // Henrique Leonardo


                                                                   // N13 - Modalidade de determinação da BC do ICMS
                                                                   //         (0)=dbiMargemValorAgregado
                                                                   //         (1)=dbiPauta
                                                                   //         (2)=dbiPrecoTabelado
                                                                   //         (3)=dbiValorOperacao

                                                                   // N18 - Modalidade de determinação da BC do ICMS ST
                                                                   //         (0)=dbisPrecoTabelado       – Preço tabelado ou máximo sugerido;
                                                                   //         (1)=dbisListaNegativa       - Lista Negativa (valor);
                                                                   //         (2)=dbisListaPositiva       - Lista Positiva (valor);
                                                                   //         (3)=dbisListaNeutra         - Lista Neutra (valor);
                                                                   //         (4)=dbisMargemValorAgregado - Margem Valor Agregado (%);
                                                                   //         (5)=dbisPauta               - Pauta (valor);)

    //                                                   TAG de grupo do IPI - <IPI> - Ocorrência 0-1

    NFe.Det[i].Imposto.IPI.clEnq := '';         // O02 - Classe de enquadramento do IPI para Cigarros e Bebidas
    NFe.Det[i].Imposto.IPI.CNPJProd := '';      // O03 - CNPJ do produtor da mercadoria, quando diferente do emitente. Somente para os casos de exportação direta ou indireta.
    NFe.Det[i].Imposto.IPI.cSelo := '';         // O04 - Código do selo de controle IPI
    NFe.Det[i].Imposto.IPI.qSelo := 0;          // O05 - Quantidade de selo de controle
    NFe.Det[i].Imposto.IPI.cEnq := '';          // O06 - Código de Enquadramento Legal do IPI (Tabela a ser criada pela RFB, informar 999 enquanto a tabela não for criada)
    NFe.Det[i].Imposto.IPI.CST := ipi00;        // O09 - Código da situação tributária do IPI
                                                //       (00)=ipi00 - Entrada com recuperação de crédito
                                                //       (49)=ipi49 - Outras entradas
                                                //       (50)=ipi50 - Saída tributada
                                                //       (99)=ipi99 - Outras saídas
                                                //       (01)=ipi01 - Entrada tributada com alíquota zero
                                                //       (02)=ipi02 - Entrada isenta
                                                //       (03)=ipi03 - Entrada não-tributada
                                                //       (04)=ipi04 - Entrada imune
                                                //       (05)=ipi05 - Entrada com suspensão
                                                //       (51)=ipi51 - Saída tributada com alíquota zero
                                                //       (52)=ipi52 - Saída isenta
                                                //       (53)=ipi53 - Saída não-tributada
                                                //       (54)=ipi54 - Saída imune
                                                //       (55)=ipi55 - Saída com suspensão

    if (NFe.Det[i].Imposto.IPI.CST = ipi00) or
    (NFe.Det[i].Imposto.IPI.CST = ipi49) or
    (NFe.Det[i].Imposto.IPI.CST = ipi50) or
    (NFe.Det[i].Imposto.IPI.CST = ipi99) then
    begin
      if opcao1 then
      begin
        NFe.Det[ i].Imposto.IPI.vBC := 0;       // O10 - Valor da BC do IPI
        NFe.Det[ i].Imposto.IPI.pIPI := 0;      // 013 - Alíquota do IPI
        NFe.Det[i].Imposto.IPI.vIPI := 0;       // O14 - Valor do IPI
      end;
      if not opcao1 then
      begin
        NFe.Det[ i].Imposto.IPI.qUnid := 0;     // O11 - Qtde total na unidade padrão para tributação (somente para os produtos tributados por unidade)
        NFe.Det[ i].Imposto.IPI.vUnid := 0;     // O12 - Valor por Unidade Tributável
        NFe.Det[i].Imposto.IPI.vIPI := 0;       // O14 - Valor do IPI
      end;
    end;

    //                                                   TAG de grupo do Imposto de Importação - <II> - Ocorrência 0-1

                                                //       Informar apenas quando o item for sujeito ao II
    NFe.Det[i].Imposto.II.vBc := 0;             // P02 - Valor da BC do Imposto de Importação
    NFe.Det[i].Imposto.II.vDespAdu := 0;        // P03 - Valor das despesas aduaneiras
    NFe.Det[i].Imposto.II.vII := 0;             // P04 - Valor do Imposto de Importação
    NFe.Det[i].Imposto.II.vIOF := 0;            // P05 - Valor do Imposto sobre Operações Financeiras

    //                                                   TAG de grupo do PIS - <PIS> - Ocorrência 1-1

    NFe.Det[i].Imposto.PIS.CST := pis01;        // Q06 - Código de Situação Tributária do PIS
                                                //         (01)=pis01 – Operação Tributável (base de cálculo = valor da operação alíquota normal (cumulativo/não cumulativo));
                                                //         (02)=pis02 - Operação Tributável (base de cálculo = valor da operação (alíquota diferenciada));
                                                //         (03)=pis03 - Operação Tributável (base de cálculo = quantidade vendida x alíquota por unidade de produto )
                                                //         (04)=pis04 - Operação Tributável (tributação monofásica (alíquota zero));
                                                //         (06)=pis06 - Operação Tributável (alíquota zero);
                                                //         (07)=pis07 - Operação Isenta da Contribuição;
                                                //         (08)=pis08 - Operação Sem Incidência da Contribuição;
                                                //         (09)=pis09 - Operação com Suspensão da Contribuição;
                                                //         (99)=pis99 - Outras Operações;

    if (NFe.Det[i].Imposto.PIS.CST = pis01) or
    (NFe.Det[i].Imposto.PIS.CST = pis02) then
    begin
      NFe.Det[i].Imposto.PIS.vBC := 0;          // Q07 - Valor da Base de Cálculo do PIS
      NFe.Det[i].Imposto.PIS.pPIS := 0;         // Q08 - Alíquota do PIS (em percentual)
      NFe.Det[i].Imposto.PIS.qBCProd := 0;      // Q10 - Quantidade Vendida
      NFe.Det[i].Imposto.PIS.vAliqProd := 0;    // Q11 - Alíquota do PIS (em reais)
      NFe.Det[i].Imposto.PIS.vPIS := 0;         // Q09 - Valor do PIS
    end;
    if (NFe.Det[i].Imposto.PIS.CST = pis03) then
    begin
      NFe.Det[i].Imposto.PIS.qBCProd := 0;      // Q10 - Quantidade Vendida
      NFe.Det[i].Imposto.PIS.vAliqProd := 0;    // Q11 - Alíquota do PIS (em reais)
      NFe.Det[i].Imposto.PIS.vPIS := 0;         // Q09 - Valor do PIS
    end;
    if (NFe.Det[i].Imposto.PIS.CST = pis03) and (opcao1)then
    begin
      NFe.Det[i].Imposto.PIS.vBC := 0;          // Q07 - Valor da Base de Cálculo do PIS
      NFe.Det[i].Imposto.PIS.pPIS := 0;         // Q08 - Alíquota do PIS (em percentual)
      NFe.Det[i].Imposto.PIS.vPIS := 0;         // Q09 - Valor do PIS
    end;
    if (NFe.Det[i].Imposto.PIS.CST = pis03) and (not opcao1)then
    begin
      NFe.Det[i].Imposto.PIS.qBCProd := 0;      // Q10 - Quantidade Vendida
      NFe.Det[i].Imposto.PIS.vAliqProd := 0;    // Q11 - Alíquota do PIS (em reais)
      NFe.Det[i].Imposto.PIS.vPIS := 0;         // Q09 - Valor do PIS
    end;

  //          TAG do grupo de PIS Substituição Tributária - <PISST> - Ocorrência 0-1

    if opcao1 then
    begin
      NFe.Det[i].Imposto.PISST.vBc := 0;        // R02 - Valor da Base de Cálculo do PIS
      NFe.Det[i].Imposto.PISST.pPis := 0;       // R03 - Alíquota do PIS (em percentual)
      NFe.Det[i].Imposto.PISST.vPIS := 0;       // R06 - Valor do PIS
    end;
    if not opcao1 then
    begin
      NFe.Det[i].Imposto.PISST.qBCProd := 0;    // R04 - Quantidade Vendida
      NFe.Det[i].Imposto.PISST.vAliqProd := 0;  // R05 - Alíquota do PIS (em reais)
      NFe.Det[i].Imposto.PISST.vPIS := 0;       // R06 - Valor do PIS
    end;

  //          TAG de grupo do COFINS - <COFINS> - Ocorrência 1-1

    NFe.Det[i].Imposto.COFINS.CST := cof01;          // S06 - Código de Situação Tributária do COFINS
                                                     //         (01) – Operação Tributável (base de cálculo = valor da operação alíquota normal (cumulativo/não cumulativo));
                                                     //         (02) - Operação Tributável (base de cálculo = valor da operação
                                                     //         (03) - Operação Tributável (base de cálculo = quantidade vendida x alíquota por unidade de produto);
                                                     //         (04) - Operação Tributável (tributação monofásica (alíquota zero));
                                                     //         (06) - Operação Tributável (alíquota zero);
                                                     //         (07) - Operação Isenta da Contribuição;
                                                     //         (08) - Operação Sem Incidência da Contribuição;
                                                     //         (09) - Operação com Suspensão da Contribuição;
                                                     //         (99) - Outras Operações;

    if (NFe.Det[i].Imposto.COFINS.CST = cof01) or (NFe.Det[i].Imposto.COFINS.CST = cof02) then
    begin
      NFe.Det[i].Imposto.COFINS.vBC := 0;            // S07 - Valor da Base de Cálculo da COFINS
      NFe.Det[i].Imposto.COFINS.pCOFINS := 0;        // S08 - Alíquota da COFINS (em percentual)
      NFe.Det[i].Imposto.COFINS.vCOFINS := 0;        // S11 - Valor do COFINS
    end;
    if NFe.Det[i].Imposto.COFINS.CST = cof03 then
    begin
      NFe.Det[i].Imposto.COFINS.vBCProd := 0;        // S09 - Valor do COFINS
      NFe.Det[i].Imposto.COFINS.vAliqProd := 0;      // S10 - Alíquota do COFINS (em reais)
      NFe.Det[i].Imposto.COFINS.vCOFINS := 0;        // S11 - Valor do COFINS
    end;
    if ( NFe.Det[i].Imposto.COFINS.CST = cof99) and (opcao1) then
    begin
      NFe.Det[i].Imposto.COFINS.vBC := 0;            // S07 - Valor da Base de Cálculo da COFINS
      NFe.Det[i].Imposto.COFINS.pCOFINS := 0;        // S08 - Alíquota da COFINS (em percentual)
      NFe.Det[i].Imposto.COFINS.vCOFINS := 0;        // S11 - Valor do COFINS
    end;
    if ( NFe.Det[i].Imposto.COFINS.CST = cof99) and (not opcao1) then
    begin
      NFe.Det[i].Imposto.COFINS.qBCProd := 0;        // S09 - Quantidade Vendida
      NFe.Det[i].Imposto.COFINS.vAliqProd := 0;      // S10 - Alíquota do COFINS (em reais)
      NFe.Det[i].Imposto.COFINS.vCOFINS := 0;        // S11 - Valor do COFINS
    end;

  //          TAG do grupo de COFINS Substituição Tributária - <COFINSST> - Ocorrência 0-1

    if opcao1 then
    begin
      NFe.Det[i].Imposto.COFINSST.vBC := 0;          // T02 - Valor da Base de Cálculo da COFINS
      NFe.Det[i].Imposto.COFINSST.pCOFINS := 0;      // T03 - Alíquota da COFINS (em percentual)
      NFe.Det[i].Imposto.COFINSST.vCOFINS := 0;      // T06 - Valor do COFINS
    end;
    if not opcao1 then
    begin
      NFe.Det[i].Imposto.COFINSST.qBCProd := 0;      // T04 - Quantidade Vendida
      NFe.Det[i].Imposto.COFINSST.vAliqProd := 0;    // T05 - Alíquota do COFINS (em reais)
      NFe.Det[i].Imposto.COFINSST.vCOFINS := 0;      // T06 - Valor do COFINS
    end;

  //          TAG do grupo do ISSQN - <ISSQN> - Ocorrência 0-1

    NFe.Det[i].Imposto.ISSQN.vBC := 0;               // U02 - Valor da Base de Cálculo do ISSQN
    NFe.Det[i].Imposto.ISSQN.vAliq := 0;             // U03 - Alíquota do ISSQN
    NFe.Det[i].Imposto.ISSQN.vISSQN := 0;            // U04 - Valor do ISSQN
    NFe.Det[i].Imposto.ISSQN.cMunFG := 0;            // U05 - Código do município de ocorrência do fato gerador do ISSQN
    NFe.Det[i].Imposto.ISSQN.cListServ := '0';         // U06 - Código da Lista de Serviços

  end;

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de Valores Totais da NF-e - <total> - Ocorrência 1-1                                                 *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                                     //       TAG de grupo de Valores Totais referentes ao ICMS - <ICMSTot> - Ocorrência 1-1
  NFe.Total.ICMSTot.vBC := 0;        // W03 - Base de Cálculo do ICMS
  NFe.Total.ICMSTot.vICMS := 0;      // W04 - Valor Total do ICMS
  NFe.Total.ICMSTot.vBCST := 0;      // W05 - Base de Cálculo do ICMS ST
  NFe.Total.ICMSTot.vST := 0;        // W06 - Valor Total do ICMS ST
  NFe.Total.ICMSTot.vProd := 0;      // W07 - Valor Total dos produtos e serviços
  NFe.Total.ICMSTot.vFrete := 0;     // W08 - Valor Total do Frete
  NFe.Total.ICMSTot.vSeg := 0;       // W09 - Valor Total do Seguro
  NFe.Total.ICMSTot.vDesc := 0;      // W10 - Valor Total do Desconto
  NFe.Total.ICMSTot.vII := 0;        // W11 - Valor Total do II
  NFe.Total.ICMSTot.vIPI := 0;       // W12 - Valor Total do IPI
  NFe.Total.ICMSTot.vPIS := 0;       // W13 - Valor do PIS
  NFe.Total.ICMSTot.vCOFINS := 0;    // W14 - Valor do COFINS
  NFe.Total.ICMSTot.vOutro := 0;     // W15 - Outras Despesas acessórias
  NFe.Total.ICMSTot.vNF := 0;        // W16 - Valor Total da NF-e

                                     //       TAG de grupo de Valores Totais referentes ao ISSQN - <ISSQNtot> - Ocorrência 0-1
  NFe.Total.ISSQNtot.vServ := 0;     // W18 - Valor Total dos Serviços sob nãoincidência ou não tributados pelo ICMS
  NFe.Total.ISSQNtot.vBC := 0;       // W19 - Base de Cálculo do ISS
  NFe.Total.ISSQNtot.vISS := 0;      // W20 - Valor Total do ISS
  NFe.Total.ISSQNtot.vPIS := 0;      // W21 - Valor do PIS sobre serviços
  NFe.Total.ISSQNtot.vCOFINS := 0;   // W22 - Valor do COFINS sobre serviços

                                     //       TAG de grupo de Retenções de Tributos - <retTrib> - Ocorrência 0-1
  NFe.Total.retTrib.vRetPIS := 0;    // W24 - Valor Retido de PIS
  NFe.Total.retTrib.vRetCOFINS := 0; // W25 - Valor Retido de COFINS
  NFe.Total.retTrib.vRetCSLL := 0;   // W26 - Valor Retido de CSLL
  NFe.Total.retTrib.vBCIRRF := 0;    // W27 - Base de Cálculo do IRRF
  NFe.Total.retTrib.vIRRF := 0;      // W28 - Valor Retido do IRRF
  NFe.Total.retTrib.vBCRetPrev := 0; // W29 - Base de Cálculo da Retenção da Previdência Social
  NFe.Total.retTrib.vRetPrev := 0;   // W30  - Valor da Retenção da Previdência Social

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de Informações do Transporte da NF-e - <transp> - Ocorrência 1-1                                     *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

  NFe.Transp.modFrete := mfContaEmitente;       // X02 - Modalidade do frete
                                                //         (0)=mfContaEmitente     – por conta do emitente;
                                                //         (1)=mfContaDestinatario – por conta do destinatário)
                                                //       TAG de grupo Transportador - <transporta> - Ocorrência 0-1
  NFe.Transp.Transporta.CNPJCPF := '';          // X05 - Informar o CNPJ ou o CPF do Transportador, preenchendo os zeros não significativos.
  NFe.Transp.Transporta.xNome := '';            // X06 - Razão Social ou nome
  NFe.Transp.Transporta.IE := '';               // X07 - Inscrição Estadual
  NFe.Transp.Transporta.xEnder := '';           // X08 - Endereço Completo
  NFe.Transp.Transporta.xMun := '';             // X09 - Nome do município
  NFe.Transp.Transporta.UF := '';               // X10 - Sigla da UF
                                                //       TAG de grupo de Retenção do ICMS do transporte - <retTransp> - Ocorrência 0-1
  NFe.Transp.retTransp.vServ := 0;              // X12 - Valor do Serviço
  NFe.Transp.retTransp.vBCRet := 0;             // X13 - BC da Retenção do ICMS
  NFe.Transp.retTransp.pICMSRet := 0;           // X14 - Alíquota da Retenção
  NFe.Transp.retTransp.vICMSRet := 0;           // X15 - Valor do ICMS Retido
  NFe.Transp.retTransp.CFOP := '';              // X16 - CFOP (Utilizar Tabela de CFOP)
  NFe.Transp.retTransp.cMunFG := 0;             // X17 - Código do município de ocorrência do fato gerador do ICMS do transporte (Tabela do IBGE)
                                                //       TAG de grupo Veículo - <veicTransp> - Ocorrência 0-1
  NFe.Transp.veicTransp.placa := '';            // X19 - Placa do Veículo
  NFe.Transp.veicTransp.UF := '';               // X20 - Sigla da UF
  NFe.Transp.veicTransp.RNTC := '';             // X21 - Registro Nacional de Transportador de Carga (ANTT)

  for i := 0 to 1 do
  begin
    NFe.Transp.Reboque.Add;                     //       TAG de grupo Reboque - <reboque> - Ocorrência 0-2
    NFe.Transp.Reboque[i].placa := '';          // X23 - Placa do Veículo
    NFe.Transp.Reboque[i].UF := '';             // X24 - Sigla da UF
    NFe.Transp.Reboque[i].RNTC := '';           // X25 - Registro Nacional de Transportador de Carga (ANTT)
  end;

  for i := 0 to 1 do
  begin
    NFe.Transp.Vol.add;                         //       TAG de grupo Volumes - <vol> - Ocorrência 0-N
    NFe.Transp.Vol[i].qVol := 0;                // X27 - Quantidade de volumes transportados
    NFe.Transp.vol[i].esp := '';                // X28 - Espécie dos volumes transportados
    NFe.Transp.Vol[i].marca := '';              // X29 - Marca dos volumes transportados
    NFe.Transp.Vol[i].nVol := '';               // X30 - Numeração dos volumes transportados
    NFe.Transp.Vol[i].pesoL := 0;               // X31 - Peso Líquido (em kg)
    NFe.Transp.Vol[i].pesoB := 0;               // X32 - Peso Bruto (em kg)
    for j := 0 to 1 do
    begin
      NFe.transp.Vol[i].lacres.add;             //       TAG de grupo de Lacres - <lacres> - Ocorrência 0-N
      NFe.transp.Vol[i].lacres[j].nLacre := ''; // X34 - Número dos Lacres
                                                //       Para Combustíveis, um Caminhão (Toco, Truck, Carreta, Bi-Tren) pode ter até 32 bocas.
                                                //       Ou seja até 32 lacres. Cada lacre pode ser um número de até 8 digitos.
    end;
  end;

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de Cobrança - <cobr> - Ocorrência 0-1                                                                *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                                  // >>>   TAG de grupo da Fatura - <fat> - Ocorrência 0-1
  NFe.Cobr.Fat.nFat := '';        // Y03 - Número da Fatura
  NFe.Cobr.Fat.vOrig := 0;        // Y04 - Valor Original da Fatura
  NFe.Cobr.Fat.vDesc := 0;        // Y05 - Valor do desconto
  NFe.Cobr.Fat.vLiq := 0;         // Y06 - Valor Líquido da Fatura
  for i := 0 to 1 do
  begin
    NFe.Cobr.Dup.Add;             // >>>   TAG de grupo da Duplicata - <dup> - Ocorrência 0-N
    NFe.Cobr.Dup[i].nDup := '';   // Y08 - Número da Duplicata
    NFe.Cobr.Dup[i].dVenc := now; // Y09 - Data de vencimento
    NFe.Cobr.Dup[i].vDup := 0;    // Y10 - Valor da duplicata
  end;

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de Informações Adicionais - <infAdic> - Ocorrência 0-1                                               *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

  NFe.InfAdic.infAdFisco := '';                // Z02 - Informações Adicionais de Interesse do Fisco
  NFe.InfAdic.infCpl := '';                    // Z03 - Informações Complementares de interesse do Contribuinte
  for i := 0 to 9 do
  begin
    NFe.InfAdic.obsCont.Add;                   // >>>   TAG de grupo do campo de uso livre do contribuinte - <obsCont> - Ocorrência 0-10
    NFe.InfAdic.obsCont[i].xCampo := '';       // Z05 - Identificação do campo (TAM : 1-20)
    NFe.InfAdic.obsCont[i].xTexto := '';       // Z06 - Conteúdo do campo (TAM : 1-60)
  end;
  for i := 0 to 9 do
  begin
    NFe.InfAdic.obsFisco.Add;                  // >>>   TAG de grupo do campo de uso livre do Fisco - <obsFisco> - Ocorrência 0-10
    NFe.InfAdic.obsFisco[i].xCampo := '';      // Z08 - Identificação do campo (TAM : 1-20)
    NFe.InfAdic.obsFisco[i].xTexto := '';      // Z09 - Conteúdo do campo (TAM : 1-60)
  end;
  for i := 0 to 1 do
  begin
    NFe.InfAdic.procRef.Add;                   // >>>   Tag de grupo do processo referenciado - <procRef> - Ocorrência 0-N
    NFe.InfAdic.procRef[i].nProc := '';        // Z08 - Indentificador do processo ou ato concessório
    NFe.InfAdic.procRef[i].indProc := ipSEFAZ; // Z09 - Indicador da origem do processo  (0 - SEFAZ; 1 - Justiça Federal; 2 - Justiça Estadual; 3 - Secex/RFB; 9 - Outros)
  end;

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG do Grupo de Exportação - <exporta> - Ocorrência 0-1                                                           *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                                //        O grupo sera gravado se um dos campos abaixo for preenchido
  NFe.exporta.UFembarq := '';   // ZA02 - Sigla da UF onde ocorrerá o Embarque dos produtos
  NFe.exporta.xLocEmbarq := ''; // ZA03 - Local onde ocorrerá o Embarque dos produtos

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG do Grupo de Compra - <compra> - Ocorrência 0-1                                                                *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                          //        O grupo sera gravado se um dos campos abaixo for preenchido
  NFe.compra.xNEmp := ''; // ZB02 - Nota de Empenho quando se tratar de compras públicas
  NFe.compra.xPed := '';  // ZB03 - Pedido
  NFe.compra.xCont := ''; // ZB04 - Contrato de Compra

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG do Assinatura - <signature>                                                                                   *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                          //        Opcionalmente pode gerar o template da assinatura - Isso não sginifica assinar o arquivo!

  NFe.signature.URI := NFe.infNFe.Id;

  (****************************************************************************)
  (*                                                                          *)
  (*                G E R A R   O   A R Q U I V O   X M L                     *)
  (*                                                                          *)
  (****************************************************************************)

  // Criar a class TNFeW para a geração do arquivo conforme os dados inseridos
  // em NFe passar o objeto que contém os dados para a geração do arquivo xml

  NFeW := TNFeW.Create(NFe);

  // Informa as opções especificas de TNFeW

//  NFew.Opcoes.GerarTagIPIparaNaoTributado := True;               // No caso de produto não tributado gera a TAG especifica
  NFeW.Opcoes.GerarTXTSimultaneamente := False;                  // Possibilita gerar um arquivo TXT do formato do programa SEFAZ-SP
  NFeW.Opcoes.NormatizarMunicipios := False;                     // Realiza a normatização do nome do municipio conforme a tabela do IBGE
  NFeW.Opcoes.PathArquivoMunicipios := 'C:\meuCaminho\MunIBGE\'; // Indicar para aonde estão os arquivo com os nomes dos municipios
  NFeW.Opcoes.GerarTagAssinatura := taNunca;                     // Opção de gerar o template da assinature em branco
//  NFeW.Opcoes.ValidarInscricoes := False;                        // Valida as Inscrições. (É melhor quando essa validação é feita no ERP)

  // Informar as opções comuns ao gerador ( Abaixo opcões Default)

  NFeW.Gerador.Opcoes.IdentarXML := False;                                                   // Os arquivos que serão enviados para o SEFAZ não devem estar identados
  NFeW.Gerador.Opcoes.TamanhoIdentacao := 3;                                                 // Tamanho da identação do arquivo
  NFeW.Gerador.Opcoes.FormatoAlerta := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'; // Formato em que a mensagem vai ser gravada a ListaDeAlertas
  NFeW.Gerador.Opcoes.RetirarEspacos := True;                                                // Retira os espaços em branco duplos nas tag do xml
  NFeW.Gerador.Opcoes.SuprimirDecimais := False;                                             // Ignora valores não significativos nas casa decimais
  NFeW.Gerador.Opcoes.SomenteValidar := False;                                               // Não gera o arquivo apenas valida as informações

  // Gerar o arquivo XML

  NFeW.GerarXml;

  // Carrega erros

  // if NFeW.Gerador.ListaDeAlertas.Count > 0 then
  //  memo1.Lines.Add(NFeW.gerador.ListaDeAlertas.Text);

  // Gravar o arquivo no HD

  if NFeW.Gerador.ListaDeAlertas.Count = 0 then // Se não contiver nenhum erro, grava
  begin
    NFeW.gerador.SalvarArquivo('C:\Meu-Caminho\' + NFeW.ObterNomeArquivo);                  // Não é necessário informar o parametro fpXML pois ele é default
    if NFeW.Opcoes.GerarTXTSimultaneamente then
      NFeW.gerador.SalvarArquivo('C:\Meu-Caminho\' + NFeW.ObterNomeArquivo + '.txt',fgTXT); // Informar o parametro fgTXT para gerar o arquivo no formato TEXTO
  end;

  NFeW.Free;
  NFe.Free;

end;

end.

