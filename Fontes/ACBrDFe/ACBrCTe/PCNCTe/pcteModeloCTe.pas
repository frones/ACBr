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

unit pcteModeloCTe;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnConversao, pcteCTe, pcteCTeW, pcteConversaoCTe,
  ACBrUtil.Strings;

procedure ModeloCTe;

implementation

procedure ModeloCTe;
var
  CTe: TCTe;
  CTeW: TCTeW;
  i{, j, k}: Integer;
//  s: String;
  ReferenciadaTipoCTe: Boolean;
  Opcao1: Boolean;
begin

  // IMPORTANTE: Os laços For - Next codificados nesses modelo são meramente descritivos.
  //             Esse arquivo é apenas um modelo e deve ser adaptado conforme as suas necessidades.

  Opcao1 := True;      // Esta variavel esta sendo usada nesse modelo para indicar os locais onde
                       // devem ser tomadas decissões por parte do programador conforme a regras
                       // de negocio de cada cliente.

  CTe := TCTe.create;

//  s := CTe.infCTe.ID;  // ATENÇÃO: Esse campo representa a chave da CTe
                       //          Não utilize esse campo para escrita (apenas para leitura)
                       //          pois a chave é gerada automaticamente no momento da geração do arquivo

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo das informações de identificação do CT-e  - <ide> - Ocorrência 1-1                                   *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

  CTe.Ide.cUF := 0;                     // - Código da UF do emitente do Documento Fiscal - Tabela do IBGE
                                        //     Você pode utilizar a função UFparaCodigo caso não sáiba o código da UF
                                        //     ex: CTe.Ide.cUF := UFparaCodigo(MinhaUF);
  CTe.Ide.cCT := -1;                    // - Código Numérico que compõe a Chave de Acesso
                                        //     Se nenhum valor for informado será atribuido um valor aleatório
                                        //     Se for informado o valor -1; será gerado um valor baseado no numero da CTe
  CTe.ide.CFOP := 0;                    // - Código Fiscal de Operações e Prestações
  CTe.Ide.natOp := '';                  // - Descrição da Natureza da Operação
  CTe.Ide.forPag := fpPago;             // - Indicador da forma de pagamento (*)
                                        //         (0)=fpPago
                                        //         (1)=fpAPagar
                                        //         (2)=fpOutras
  CTe.Ide.modelo := 57;                 // - Código do Modelo do Documento Fiscal Utilizar o código 57 para identificação do CT-e.
  CTe.Ide.serie := 0;                   // - Série do Documento Fiscal, informar 0 (zero) para série única.
  CTe.Ide.nCT := 0;                     // - Número do Documento Fiscal
  CTe.Ide.dhEmi := now;                 // - Data e Hora de emissão do Documento Fiscal
  CTe.Ide.tpImp := tiRetrato;           // - Formato de Impressão do DACTe (*)
                                        //         (1)=tiRetrato
                                        //         (2)=tiPaisagem
  CTe.Ide.tpEmis := teNormal;           // - Forma de Emissão do CT-e (*)
                                        //         (1)=teNormal
                                        //         (4)=teEPEC
                                        //         (5)=teFSDA
                                        //         (7)=teSVCRS
                                        //         (8)=teSVCSP
  CTe.Ide.tpAmb := taProducao;          // - Identificação do Ambiente (*)
                                        //         (1)=Produção
                                        //         (2)=Homologação
  CTe.Ide.tpCTe := tcNormal;            // - Tipo do Documento Fiscal (*)
                                        //         (0)=tcNormal
                                        //         (1)=tcComplemento
                                        //         (2)=tcAnulacao
                                        //         (3)=tcSubstituto
  CTe.Ide.procEmi :=                    // - Processo de emissão do CT-e (*)
    peAplicativoContribuinte;           //         (0)=peAplicativoContribuinte      - emissão do CT-e com aplicativo do contribuinte;
                                        //         (1)=peAvulsaFisco                 - emissão do CT-e avulsa pelo Fisco;
                                        //         (2)=peAvulsaContribuinte          - emissão do CT-e avulsa, pelo contribuinte com seu certificado digital, através do site do Fisco;
                                        //         (3)=peContribuinteAplicativoFisco - emissão do CT-e pelo contribuinte com aplicativo fornecido pelo Fisco.
  CTe.Ide.verProc := '';                // - Versão do Processo de emissão do CT-e
  ReferenciadaTipoCTe := True;          // TAG - Informação das NF/NF-e referenciadas - <NFref> - Ocorrência 0-N ********
  if ReferenciadaTipoCTe then
  begin                                 // Se a nota referenciada for um CTe preencher o campo abaixo:
    CTe.Ide.refCTe := '';               // - Chave de acesso do CT-e referenciado
  end;
  CTe.Ide.cMunEnv := 0;                 // - Código do Municipio de Envio
  CTe.Ide.xMunEnv := '';                // - Nome do Municipio de Envio
  CTe.Ide.UFEnv   := '';                // - UF do Municipio de Envio
  CTe.Ide.modal   := mdRodoviario;      // - Modal
                                        //   mdRodoviario
                                        //   mdAereo
                                        //   mdAquaviario
                                        //   mdFerroviario
                                        //   mdDutoviario
  CTe.Ide.tpServ := tsNormal;           // - Tipo de Serviço
                                        //   tsNormal
                                        //   tsSubcontratacao
                                        //   tsRedespacho
                                        //   tsIntermediario
  CTe.Ide.cMunIni := 0;                 // - Código do Municipio de Inicio da Prestação
  CTe.Ide.xMunIni := '';                // - Nome do Municipio de Inicio da Prestação
  CTe.Ide.UFIni   := '';                // - UF do Municipio de Inicio da Prestação
  CTe.Ide.cMunFim := 0;                 // - Código do Municipio de Fim da Prestação
  CTe.Ide.xMunFim := '';                // - Nome do Municipio de Fim da Prestação
  CTe.Ide.UFFim   := '';                // - UF do Municipio de Fim da Prestação
  CTe.Ide.retira  := rtSim;             // - Indica se o Destinatário vai retirar a caraga ou não
                                        //   rtSim
                                        //   rtNao
  CTe.Ide.xdetretira := '';             // - Detalhamento da retirada da carga

  if Opcao1 then
  begin
    // Se o Tomador do Serviço do CTe for <> de Outros
    CTe.Ide.Toma03.Toma := tmRemetente;   // - Indica que é o tomador do Serviço
                                          //   tmRemetente
                                          //   tmExpedidor
                                          //   tmRecebedor
                                          //   tmDestinatario
  end
  else
  begin
    // Se o Tomador do Serviço do CTe for = a Outros
    CTe.Ide.Toma4.Toma := tmOutros;
    CTe.Ide.Toma4.CNPJCPF := '';
    CTe.Ide.Toma4.IE := '';
    CTe.Ide.Toma4.xNome := '';
    CTe.Ide.Toma4.xFant := '';
    CTe.Ide.Toma4.fone := '';
    CTe.Ide.Toma4.EnderToma.xLgr := '';
    CTe.Ide.Toma4.EnderToma.nro := '';
    CTe.Ide.Toma4.EnderToma.xCpl := '';
    CTe.Ide.Toma4.EnderToma.xBairro := '';
    CTe.Ide.Toma4.EnderToma.cMun := 0;
    CTe.Ide.Toma4.EnderToma.xMun := '';
    CTe.Ide.Toma4.EnderToma.CEP := 0;
    CTe.Ide.Toma4.EnderToma.UF := '';
    CTe.Ide.Toma4.EnderToma.cPais := 0;
    CTe.Ide.Toma4.EnderToma.xPais := '';
  end;

  if Opcao1 then
  begin
    CTe.Ide.dhCont := Now;
    CTe.Ide.xJust  := '';
  end;

  CTe.compl.xCaracAd  := '';
  CTe.compl.xCaracSer := '';
  CTe.compl.xEmi      := '';
  CTe.compl.fluxo.xOrig := '';

  if Opcao1 then
  begin
    with CTe.compl.fluxo.pass.New do
    begin
      xPass := '';
    end;
  end;
  CTe.compl.fluxo.xDest := '';
  CTe.compl.fluxo.xRota := '';

  CTe.compl.Entrega.TipoData := tdSemData;
  case CTe.compl.Entrega.TipoData of
    tdSemData: CTe.compl.Entrega.semData.tpPer := tdSemData;
    tdNaData,
    tdAteData,
    tdApartirData: begin
                     CTe.compl.Entrega.comData.tpPer := CTe.compl.Entrega.TipoData;
                     CTe.compl.Entrega.comData.dProg := Date;
                   end;
    tdNoPeriodo: begin
                   CTe.compl.Entrega.noPeriodo.tpPer := tdNoPeriodo;
                   CTe.compl.Entrega.noPeriodo.dIni  := Date;
                   CTe.compl.Entrega.noPeriodo.dFim  := Date;
                 end;
  end;

  CTe.compl.Entrega.TipoHora := thSemHorario;
  case CTe.compl.Entrega.TipoHora of
    thSemHorario: CTe.compl.Entrega.semHora.tpHor := thSemHorario;
    thNoHorario,
    thAteHorario,
    thApartirHorario: begin
                        CTe.compl.Entrega.comHora.tpHor := CTe.compl.Entrega.TipoHora;
                        CTe.compl.Entrega.comHora.hProg := Time;
                      end;
    thNoIntervalo: begin
                     CTe.compl.Entrega.noInter.tpHor := thNoIntervalo;
                     CTe.compl.Entrega.noInter.hIni  := Time;
                     CTe.compl.Entrega.noInter.hFim  := Time;
                   end;
  end;

  CTe.compl.origCalc := '';
  CTe.compl.destCalc := '';
  CTe.compl.xObs     := '';

  if Opcao1 then
  begin
    with CTe.compl.ObsCont.New do
    begin
      xCampo := '';
      xTexto := '';
    end;
  end;

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do emitente do CT-e  - <emit> - Ocorrência 1-1                                      *)
  (* ----------------------------------------------------------------------------------------------------------------- *)
  CTe.Emit.CNPJ := '';              // - CNPJ do emitente
  CTe.Emit.xNome := '';             // - Razão Social ou Nome do emitente
  CTe.Emit.xFant := '';             // - Nome fantasia
  CTe.Emit.IE := '';                // - Inscrição Estadual do emitente
  CTe.Emit.enderEmit.xLgr := '';    // - Logradouro
  CTe.Emit.enderEmit.nro := '';     // - Número
  CTe.Emit.enderEmit.xCpl := '';    // - Complemento
  CTe.Emit.enderEmit.xBairro := ''; // - Bairro
  CTe.Emit.enderEmit.cMun := 0;     // - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  CTe.Emit.enderEmit.xMun := '';    // - Nome do município   ('EXTERIOR' para operações com o exterior)
  CTe.Emit.enderEmit.UF := '';      // - Sigla da UF         ('EX' para operações com o exterior.)
  CTe.Emit.enderEmit.CEP := 0;      // - Código do CEP
  CTe.Emit.enderEmit.fone := '';    // - Telefone            ( Código DDD + número do telefone. )

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do remetente do CT-e  - <rem> - Ocorrência 0-1                                      *)
  (* ----------------------------------------------------------------------------------------------------------------- *)
  CTe.Rem.CNPJCPF := '';           // - CNPJ/CPF do remetente
  CTe.Rem.xNome := '';             // - Razão Social ou Nome
  CTe.Rem.xFant := '';             // - Nome fantasia
  CTe.Rem.IE := '';                // - Inscrição Estadual
  CTe.Rem.fone := '';              // - Telefone            ( Código DDD + número do telefone. )
  CTe.Rem.email := '';             // - e-mail
  CTe.Rem.enderReme.xLgr := '';    // - Logradouro
  CTe.Rem.enderReme.nro := '';     // - Número
  CTe.Rem.enderReme.xCpl := '';    // - Complemento
  CTe.Rem.enderReme.xBairro := ''; // - Bairro
  CTe.Rem.enderReme.cMun := 0;     // - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  CTe.Rem.enderReme.xMun := '';    // - Nome do município   ('EXTERIOR' para operações com o exterior)
  CTe.Rem.enderReme.UF := '';      // - Sigla da UF         ('EX' para operações com o exterior.)
  CTe.Rem.enderReme.CEP := 0;      // - Código do CEP

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do Local de coleta - <locColeta> - Ocorrência 0-1                                   *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                              // Informar os valores desse grupo somente se o  endereço de
                              // retirada for diferente do endereço do remetente.
                              // Assim se locColeta.xLgr <> '' o gerador grava o grupo no XML

  CTe.Rem.locColeta.CNPJCPF := ''; // - CNPJ/CPF
  CTe.Rem.locColeta.xNome := '';   // - Nome
  CTe.Rem.locColeta.xLgr := '';    // - Logradouro
  CTe.Rem.locColeta.nro := '';     // - Número
  CTe.Rem.locColeta.xCpl := '';    // - Complemento
  CTe.Rem.locColeta.xBairro := ''; // - Bairro
  CTe.Rem.locColeta.cMun := 0;     // - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  CTe.Rem.locColeta.xMun := '';    // - Nome do município   ('EXTERIOR' para operações com o exterior)
  CTe.Rem.locColeta.UF := '';      // - Sigla da UF         ('EX' para operações com o exterior.)

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do destinatário do CT-e  - <dest> - Ocorrência 0-1                                  *)
  (* ----------------------------------------------------------------------------------------------------------------- *)
  CTe.Dest.CNPJCPF := '';           // - CNPJ/CPF do Destinatário
  CTe.Dest.xNome := '';             // - Razão Social ou Nome
  CTe.Dest.IE := '';                // - Inscrição Estadual
  CTe.Dest.fone := '';              // - Telefone            ( Código DDD + número do telefone. )
  CTe.Dest.email := '';             // - e-mail
  CTe.Dest.ISUF := '';              // - Inscrição no SUFRAMA
  CTe.Dest.enderDest.xLgr := '';    // - Logradouro
  CTe.Dest.enderDest.nro := '';     // - Número
  CTe.Dest.enderDest.xCpl := '';    // - Complemento
  CTe.Dest.enderDest.xBairro := ''; // - Bairro
  CTe.Dest.enderDest.cMun := 0;     // - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  CTe.Dest.enderDest.xMun := '';    // - Nome do município   ('EXTERIOR' para operações com o exterior)
  CTe.Dest.enderDest.UF := '';      // - Sigla da UF         ('EX' para operações com o exterior.)
  CTe.Dest.enderDest.CEP := 0;      // - Código do CEP

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do Local de Entrega - <locEnt> - Ocorrência 0-1                                   *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

                              // Informar os valores desse grupo somente se o  endereço de
                              // Entrega for diferente do endereço do Destinatário.
                              // Assim se locEnt.xLgr <> '' o gerador grava o grupo no XML

  CTe.Dest.locEnt.CNPJCPF := ''; // - CNPJ/CPF
  CTe.Dest.locEnt.xNome := '';   // - Nome
  CTe.Dest.locEnt.xLgr := '';    // - Logradouro
  CTe.Dest.locEnt.nro := '';     // - Número
  CTe.Dest.locEnt.xCpl := '';    // - Complemento
  CTe.Dest.locEnt.xBairro := ''; // - Bairro
  CTe.Dest.locEnt.cMun := 0;     // - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  CTe.Dest.locEnt.xMun := '';    // - Nome do município   ('EXTERIOR' para operações com o exterior)
  CTe.Dest.locEnt.UF := '';      // - Sigla da UF         ('EX' para operações com o exterior.)

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do Expedidor do CT-e  - <Exped> - Ocorrência 0-1                                  *)
  (* ----------------------------------------------------------------------------------------------------------------- *)
  CTe.Exped.CNPJCPF := '';           // - CNPJ/CPF do Expedidor
  CTe.Exped.xNome := '';             // - Razão Social ou Nome
  CTe.Exped.IE := '';                // - Inscrição Estadual
  CTe.Exped.fone := '';              // - Telefone            ( Código DDD + número do telefone. )
  CTe.Exped.email := '';             // - e-mail
  CTe.Exped.enderExped.xLgr := '';    // - Logradouro
  CTe.Exped.enderExped.nro := '';     // - Número
  CTe.Exped.enderExped.xCpl := '';    // - Complemento
  CTe.Exped.enderExped.xBairro := ''; // - Bairro
  CTe.Exped.enderExped.cMun := 0;     // - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  CTe.Exped.enderExped.xMun := '';    // - Nome do município   ('EXTERIOR' para operações com o exterior)
  CTe.Exped.enderExped.UF := '';      // - Sigla da UF         ('EX' para operações com o exterior.)
  CTe.Exped.enderExped.CEP := 0;      // - Código do CEP

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG de grupo de identificação do Recebedor do CT-e  - <Receb> - Ocorrência 0-1                                  *)
  (* ----------------------------------------------------------------------------------------------------------------- *)
  CTe.Receb.CNPJCPF := '';           // - CNPJ/CPF do Recebedor
  CTe.Receb.xNome := '';             // - Razão Social ou Nome
  CTe.Receb.IE := '';                // - Inscrição Estadual
  CTe.Receb.fone := '';              // - Telefone            ( Código DDD + número do telefone. )
  CTe.Receb.email := '';             // - e-mail
  CTe.Receb.enderReceb.xLgr := '';    // - Logradouro
  CTe.Receb.enderReceb.nro := '';     // - Número
  CTe.Receb.enderReceb.xCpl := '';    // - Complemento
  CTe.Receb.enderReceb.xBairro := ''; // - Bairro
  CTe.Receb.enderReceb.cMun := 0;     // - Código do município (Tabela do IBGE - '9999999' para operações com o exterior))
  CTe.Receb.enderReceb.xMun := '';    // - Nome do município   ('EXTERIOR' para operações com o exterior)
  CTe.Receb.enderReceb.UF := '';      // - Sigla da UF         ('EX' para operações com o exterior.)
  CTe.Receb.enderReceb.CEP := 0;      // - Código do CEP

  //
  //  Valores da Prestação de Serviço
  //
  CTe.vPrest.vTPrest := 0.0;
  CTe.vPrest.vRec    := 0.0;

  //
  // Relação dos Componentes da Prestação de Serviço
  //
  for i := 0 to 1 do
  begin
    with CTe.vPrest.comp.New do
    begin
      xNome := '';
      vComp := 0.0;
    end;
  end;

  //
  //  Valores dos Impostos
  //
  CTe.Imp.ICMS.SituTrib := cst00;
  case CTe.Imp.ICMS.SituTrib of
   cst00: begin
            CTe.Imp.ICMS.SituTrib     := cst00;
            CTe.Imp.ICMS.ICMS00.CST   := cst00; // Tributação Normal ICMS
            CTe.Imp.ICMS.ICMS00.vBC   := 0.0;
            CTe.Imp.ICMS.ICMS00.pICMS := 0.0;
            CTe.Imp.ICMS.ICMS00.vICMS := 0.0;
          end;
   cst20: begin
            CTe.Imp.ICMS.SituTrib      := cst20;
            CTe.Imp.ICMS.ICMS20.CST    := cst20; // Tributação com BC reduzida do ICMS
            CTe.Imp.ICMS.ICMS20.pRedBC := 0.0;
            CTe.Imp.ICMS.ICMS20.vBC    := 0.0;
            CTe.Imp.ICMS.ICMS20.pICMS  := 0.0;
            CTe.Imp.ICMS.ICMS20.vICMS  := 0.0;
          end;
   cst40: begin
            CTe.Imp.ICMS.SituTrib  := cst40;
            CTe.Imp.ICMS.ICMS45.CST := cst40; // ICMS Isento
          end;
   cst41: begin
            CTe.Imp.ICMS.SituTrib  := cst41;
            CTe.Imp.ICMS.ICMS45.CST := cst41; // ICMS não Tributada
          end;
   cst51: begin
            CTe.Imp.ICMS.SituTrib  := cst51;
            CTe.Imp.ICMS.ICMS45.CST := cst51; // ICMS diferido
          end;
   cst60: begin
            CTe.Imp.ICMS.SituTrib          := cst60;
            CTe.Imp.ICMS.ICMS60.CST        := cst60; // Tributação atribuida ao tomador ou 3. por ST
            CTe.Imp.ICMS.ICMS60.vBCSTRet   := 0.0;
            CTe.Imp.ICMS.ICMS60.pICMSSTRet := 0.0;
            CTe.Imp.ICMS.ICMS60.vICMSSTRet := 0.0;
            CTe.Imp.ICMS.ICMS60.vCred      := 0.0;
          end;
   cst90: begin
            CTe.Imp.ICMS.SituTrib      := cst90;
            CTe.Imp.ICMS.ICMS90.CST    := cst90; // ICMS Outros
            CTe.Imp.ICMS.ICMS90.pRedBC := 0.0;
            CTe.Imp.ICMS.ICMS90.vBC    := 0.0;
            CTe.Imp.ICMS.ICMS90.pICMS  := 0.0;
            CTe.Imp.ICMS.ICMS90.vICMS  := 0.0;
            CTe.Imp.ICMS.ICMS90.vCred  := 0.0;
          end;
  end;

  // Valor Total dos Tributos
  CTe.imp.vTotTrib := 0.0;

  // Obs do Contribuinte
  if (CTe.imp.vTotTrib <> 0.0) then
  begin
    with CTe.compl.ObsCont.New do
    begin
      xCampo := 'Lei da Transparencia';
      xTexto := 'O valor aproximado de tributos incidentes sobre o preço deste servico e de R$ ' +
                FormatFloat(',0.00', CTe.imp.vTotTrib) + ' (' +
                FormatFloat(',0.00', 0.0) + '%) ' +
                'Fonte: IBPT';
    end;
  end;

  case CTe.Ide.tpCTe of
    tcNormal,
    tcSubstituto: begin
                    //  Informações da Carga
                    CTe.infCTeNorm.infCarga.vCarga  := 0.0;
                    CTe.infCTeNorm.infCarga.proPred := '';
                    CTe.infCTeNorm.infCarga.xOutCat := '';
                    // UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS);
                    with CTe.infCTeNorm.infCarga.InfQ.New do
                    begin
                      cUnid  := uKg;
                      tpMed  := 'Kg';
                      qCarga := 0.0;
                    end;
                    //  Informações da Seguradora
                    with CTe.infCTeNorm.seg.New do
                    begin
                      respSeg := rsRemetente;
                      //respSeg := rsExpedidor;
                      //respSeg := rsRecebedor;
                      //respSeg := rsDestinatario;
                      //respSeg := rsEmitenteCTe;
                      //respSeg := rsTomadorServico;
                      xSeg  := '';
                      nApol := '';
                      nAver := '';
                    end;
                    //  Dados do Modal Rodoviário
                    CTe.infCTeNorm.rodo.RNTRC := '';
                    CTe.infCTeNorm.Rodo.dPrev := Date;
                    CTe.infCTeNorm.Rodo.Lota := ltNao;
                    //CTe.infCTeNorm.Rodo.Lota := ltSim;
                    CTe.infCTeNorm.Rodo.CIOT := '';
                  end;
    tcComplemento: begin
                     CTe.infCteComp.chave := '';
                   end;
    tcAnulacao: begin
                  CTe.infCTeAnu.chCTe := '';
                  CTe.infCTeAnu.dEmi  := Date;
                end;
  end;

  for i := 0 to 1 do
  begin
    with CTe.autXML.New do
    begin
      CNPJCPF := '';
    end;
  end;

  (* ----------------------------------------------------------------------------------------------------------------- *)
  (* TAG do Assinatura - <signature>                                                                                   *)
  (* ----------------------------------------------------------------------------------------------------------------- *)

  // Opcionalmente pode gerar o template da assinatura - Isso não sginifica assinar o arquivo!

  CTe.signature.URI := CTe.infCTe.Id;

  (****************************************************************************)
  (*                                                                          *)
  (*                G E R A R   O   A R Q U I V O   X M L                     *)
  (*                                                                          *)
  (****************************************************************************)

  // Criar a class TCTeW para a geração do arquivo conforme os dados inseridos
  // em CTe passar o objeto que contém os dados para a geração do arquivo xml

  CTeW := TCTeW.Create(CTe);

  // Informa as opções especificas de TCTeW

  CTeW.Opcoes.NormatizarMunicipios := False;                     // Realiza a normatização do nome do municipio conforme a tabela do IBGE
  CTeW.Opcoes.PathArquivoMunicipios := 'C:\meuCaminho\MunIBGE\'; // Indicar para aonde estão os arquivo com os nomes dos municipios
  CTeW.Opcoes.GerarTagAssinatura := taNunca;                     // Opção de gerar o template da assinature em branco
  CTeW.Opcoes.ValidarInscricoes := False;                        // Valida as Inscrições. (É melhor quando essa validação é feita no ERP)

  // Informar as opções comuns ao gerador ( Abaixo opcões Default)

  CTeW.Gerador.Opcoes.IdentarXML := False;                                                   // Os arquivos que serão enviados para o SEFAZ não devem estar identados
  CTeW.Gerador.Opcoes.TamanhoIdentacao := 3;                                                 // Tamanho da identação do arquivo
  CTeW.Gerador.Opcoes.FormatoAlerta := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'; // Formato em que a mensagem vai ser gravada a ListaDeAlertas
  CTeW.Gerador.Opcoes.RetirarEspacos := True;                                                // Retira os espaços em branco duplos nas tag do xml
  CTeW.Gerador.Opcoes.SuprimirDecimais := False;                                             // Ignora valores não significativos nas casa decimais
  CTeW.Gerador.Opcoes.SomenteValidar := False;                                               // Não gera o arquivo apenas valida as informações

  // Gerar o arquivo XML

  CTeW.GerarXml;

  // Carrega erros

  // if CTeW.Gerador.ListaDeAlertas.Count > 0 then
  //  memo1.Lines.Add(CTeW.gerador.ListaDeAlertas.Text);

  // Gravar o arquivo no HD

  if CTeW.Gerador.ListaDeAlertas.Count = 0 then // Se não contiver nenhum erro, grava
  begin
    CTeW.gerador.SalvarArquivo('C:\Meu-Caminho\' + OnlyNumber(CTeW.CTe.infCTe.Id) + '-cte.xml'); // Não é necessário informar o parametro fpXML pois ele é default
  end;

  CTeW.Free;
  CTe.Free;

end;

end.

