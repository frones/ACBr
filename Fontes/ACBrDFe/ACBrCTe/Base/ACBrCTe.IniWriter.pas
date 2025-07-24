{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrCTe.IniWriter;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrCTe.Classes,
  pcteProcCTe,
  pcnConversao,
  pcteConversaoCTe;

type
  { TCTeIniWriter }

  TCTeIniWriter = class
  private
    FCTe: TCTe;

    procedure Gerar_CTe(AINIRec: TMemIniFile);
    procedure Gerar_CTeSimp(AINIRec: TMemIniFile);
    procedure Gerar_CTeOS(AINIRec: TMemIniFile);
    procedure Gerar_GTVe(AINIRec: TMemIniFile);

    procedure Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Gerar_Tomador4(AINIRec: TMemIniFile; toma4: TToma4);
    procedure Gerar_Complemento(AINIRec: TMemIniFile; Compl: TCompl);
    procedure Gerar_ComplementoPassagem(AINIRec: TMemIniFile; pass: TPassCollection);
    procedure Gerar_ComplementoObsCont(AINIRec: TMemIniFile; ObsCont: TObsContCollection);
    procedure Gerar_ComplementoObsFisco(AINIRec: TMemIniFile; ObsFisco: TObsFiscoCollection);
    procedure Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Gerar_Remetente(AINIRec: TMemIniFile; Rem: TRem);
    procedure Gerar_LocalColeta(AINIRec: TMemIniFile; locColeta: TLocColeta);
    procedure Gerar_Expedidor(AINIRec: TMemIniFile; exped: TExped);
    procedure Gerar_Recebedor(AINIRec: TMemIniFile; receb: TReceb);
    procedure Gerar_Destinatario(AINIRec: TMemIniFile; dest: TDest);
    procedure Gerar_ValorPrestacao(AINIRec: TMemIniFile; vPrest: TVPrest);
    procedure Gerar_ValorPrestacaoComposicao(AINIRec: TMemIniFile; Comp: TCompCollection);
    procedure Gerar_Imposto(AINIRec: TMemIniFile; Imp: TImp);
    procedure Gerar_InfCTeNormal(AINIRec: TMemIniFile; infCTeNorm: TInfCTeNorm);
    procedure Gerar_InfCarga(AINIRec: TMemIniFile; infCarga: TInfCarga);
    procedure Gerar_InfCTeNormalInfNF(AINIRec: TMemIniFile; infNF: TInfNFCollection);
    procedure Gerar_InfCTeNormalInfNFe(AINIRec: TMemIniFile; infNFe: TInfNFeCollection);
    procedure Gerar_InfCTeNormalInfOutros(AINIRec: TMemIniFile; infOutros: TInfOutrosCollection);
    procedure Gerar_InfCTeNormalInfDCe(AINIRec: TMemIniFile; infDCe: TInfDCeCollection);
    procedure Gerar_InfCTeNormalDocAnteriores(AINIRec: TMemIniFile; docAnt: TDocAnt);
    procedure Gerar_InfCTeNormalInfModal(AINIRec: TMemIniFile; infCTeNorm: TInfCTeNorm);

    procedure Gerar_InfModalRodoviario(AINIRec: TMemIniFile; rodo: TRodo);
    procedure Gerar_InfModalAereo(AINIRec: TMemIniFile; aereo: TAereo);
    procedure Gerar_Peri(AINIRec: TMemIniFile; peri: TPeriCollection);
    procedure Gerar_infTotAP(AINIRec: TMemIniFile; peri: TPeriCollection; Idx: Integer);


    procedure Gerar_InfModalAquav(AINIRec: TMemIniFile; aquav: TAquav);
    procedure Gerar_InfModalFerrov(AINIRec: TMemIniFile; ferrov: TFerrov);
    procedure Gerar_InfModalDuto(AINIRec: TMemIniFile; duto: TDuto);

    procedure Gerar_InfCTeNormalVeiculosNovos(AINIRec: TMemIniFile; veicNovos: TVeicNovosCollection);
    procedure Gerar_InfCTeNormalCobr(AINIRec: TMemIniFile; cobr: TCobr);
    procedure Gerar_InfCTeNormalInfCTeSub(AINIRec: TMemIniFile; infCteSub: TInfCteSub);
    procedure Gerar_InfCTeNormalInfGlobalizado(AINIRec: TMemIniFile; infGlobalizado: TInfGlobalizado);
    procedure Gerar_InfCTeNormalInfCTeMultimodal(AINIRec: TMemIniFile; infServVinc: TInfServVinc);
    procedure Gerar_InfCTeComplemento(AINIRec: TMemIniFile; infCteComp: TInfCteComp);
    procedure Gerar_InfCTeComplemento10(AINIRec: TMemIniFile; infCteComp10: TInfCteCompCollection);

    procedure Gerar_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Gerar_InfRespTecnico(AINIRec: TMemIniFile; infRespTec: TinfRespTec);
    procedure Gerar_InfCTeSupl(AINIRec: TMemIniFile; infCTeSupl: TinfCTeSupl);

    procedure Gerar_Tomador(AINIRec: TMemIniFile; toma: TToma);
    procedure Gerar_Detalhamento(AINIRec: TMemIniFile; det: TdetCollection);
    procedure Gerar_Secao_InfNFe(AINIRec: TMemIniFile; infNFe: TInfNFeCollection;
      const Secao: string; Idx1, Idx2: Integer);


    procedure Gerar_Total(AINIRec: TMemIniFile; total: Ttotal);

    procedure Gerar_InfPercurso(AINIRec: TMemIniFile; infPercurso: TinfPercursoCollection);
    procedure Gerar_InfServico(AINIRec: TMemIniFile; infServico: TInfServico);
    procedure Gerar_InfDocReferencia(AINIRec: TMemIniFile; infDocRef: TinfDocRefCollection);
    procedure Gerar_Seguro(AINIRec: TMemIniFile; seg: TSegCollection);
    procedure Gerar_InfModalRodoviarioOS(AINIRec: TMemIniFile; rodoOS: TRodoOS);
    procedure Gerar_InfCTeSub(AINIRec: TMemIniFile; infCteSub: TInfCteSub);
    procedure Gerar_InfGTVe(AINIRec: TMemIniFile; infGTVe: TinfGTVeCollection);

    procedure Gerar_Origem(AINIRec: TMemIniFile; origem: TEnderEmit);
    procedure Gerar_Destino(AINIRec: TMemIniFile; destino: TEnderEmit);
    procedure Gerar_DetalhamentoGTV(AINIRec: TMemIniFile; detGTV: TdetGTV);

    procedure Gerar_ProcessamentoCTe(AINIRec: TMemIniFile; procCTe: TprocCTe);

    // Reforma Tributária
    procedure Gerar_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS);
    procedure Gerar_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS);

    procedure Gerar_IBSCBS_gIBSCBS_gIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUFValores);
    procedure Gerar_IBSCBS_gIBSCBS_gIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMunValores);
    procedure Gerar_IBSCBS_gIBSCBS_gCBS(AINIRec: TMemIniFile; gCBS: TgCBSValores);

    procedure Gerar_IBSCBS_gIBSCBS_gTribReg(AINIRec: TMemIniFile; gTribRegular: TgTribRegular);
    procedure Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec: TMemIniFile; gIBSCredPres: TgIBSCBSCredPres;
      const Grupo: string);
    procedure Gerar_IBSCBS_gIBSCBS_gTribCompraGov(AINIRec: TMemIniFile; gTribCompraGov: TgTribCompraGov);
  public
    constructor Create(AOwner: TCTe); reintroduce;

    function GravarIni: string;

    property CTe: TCTe read FCTe write FCTe;
  end;


implementation

uses
  ACBrXmlBase,
  ACBrDFeUtil,
  ACBrDFe.Conversao,
  ACBrCTe,
  ACBrUtil.Base;

{ TCTeIniWriter }

constructor TCTeIniWriter.Create(AOwner: TCTe);
begin
  inherited Create;

  FCTe := AOwner;
end;

function TCTeIniWriter.GravarIni: string;
var
  INIRec: TMemIniFile;
  IniCTe: TStringList;
  Ok: Boolean;
begin
  Result := '';

  if not ValidarChave(FCTe.infCTe.ID) then
    raise EACBrCTeException.Create('CTe Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    INIRec.WriteString('infCTe', 'versao', VersaoCTeToStr(DblToVersaoCTe(Ok, FCTe.infCTe.versao)));

    Gerar_Identificacao(INIRec, FCTe.Ide);

    case FCTe.Ide.modelo of
      57:
        begin
          if FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
            Gerar_CTeSimp(INIRec)
          else
            Gerar_CTe(INIRec);
        end;
      64: Gerar_GTVe(INIRec);
      67: Gerar_CTeOS(INIRec);
    end;

    Gerar_ProcessamentoCTe(INIRec, FCTe.procCTe);

    IniCTe := TStringList.Create;
    try
      INIRec.GetStrings(IniCTe);
      Result := StringReplace(IniCTe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniCTe.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

procedure TCTeIniWriter.Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  Ok: Boolean;
begin
  AINIRec.WriteInteger('ide', 'cCT', Ide.cCT);
  AINIRec.WriteInteger('ide', 'CFOP', Ide.CFOP);
  AINIRec.WriteString('ide', 'natOp', Ide.natOp);
  AINIRec.WriteString('ide', 'forPag', tpforPagToStr(Ide.forPag));
  AINIRec.WriteInteger('ide', 'mod', Ide.modelo);
  AINIRec.WriteInteger('ide', 'serie', Ide.serie);
  AINIRec.WriteInteger('ide', 'nCT', Ide.nCT);
  AINIRec.WriteString('ide', 'dhEmi', DateToStr(Ide.dhEmi));
  AINIRec.WriteString('ide', 'tpImp', TpImpToStr(Ide.tpImp));
  AINIRec.WriteString('ide', 'tpEmis', TpEmisToStr(Ide.tpEmis));
  AINIRec.WriteString('ide', 'tpAmb', TpAmbToStr(ide.tpAmb));
  AINIRec.WriteString('ide', 'procEmi', procEmiToStr(Ide.procEmi));
  AINIRec.WriteString('ide', 'verProc', Ide.verProc);
  AINIRec.WriteString('ide', 'dhCont', DateToStr(Ide.dhCont));
  AINIRec.WriteString('ide', 'xJust', Ide.xJust);
  AINIRec.WriteString('ide', 'tpCTe', tpCTePagToStr(Ide.tpCTe));
  AINIRec.WriteString('ide', 'refCTe', Ide.refCTe);
  AINIRec.WriteInteger('ide', 'cMunEnv', Ide.cMunEnv);
  AINIRec.WriteString('ide', 'xMunEnv', Ide.xMunEnv);
  AINIRec.WriteString('ide', 'UFEnv', Ide.UFEnv);
  AINIRec.WriteString('ide', 'modal', TpModalToStr(Ide.modal));
  AINIRec.WriteString('ide', 'tpServ', TpServPagToStr(Ide.tpServ));

  if not (ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl]) then
  begin
    AINIRec.WriteInteger('ide', 'cMunIni', Ide.cMunIni);
    AINIRec.WriteString('ide', 'xMunIni', Ide.xMunIni);
  end;

  AINIRec.WriteString('ide', 'UFIni', Ide.UFIni);

  if not (ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl]) then
  begin
    AINIRec.WriteInteger('ide', 'cMunFim', Ide.cMunFim);
    AINIRec.WriteString('ide', 'xMunFim', Ide.xMunFim);
  end;

  AINIRec.WriteString('ide', 'UFFim', Ide.UFFim);
  AINIRec.WriteString('ide', 'retira', TpRetiraPagToStr(Ide.retira));
  AINIRec.WriteString('ide', 'xDetRetira', Ide.xDetRetira);

  if not (ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl]) then
  begin
    AINIRec.WriteString('ide','indGlobalizado', TIndicadorToStr(Ide.indGlobalizado));
    AINIRec.WriteString('ide','indIEToma', indIEDestToStr(Ide.indIEToma));
  end;

  AINIRec.WriteInteger('ide', 'cUF', ide.cUF);

  //CT-e OS GTV-e
  if StrToModeloCTe(Ok, IntToStr(Ide.modelo)) = moGTVe then
  begin
    AINIRec.WriteString('ide', 'dhSaidaOrig', DateToStr(Ide.dhSaidaOrig));
    AINIRec.WriteString('ide', 'dhChegadaDest', DateToStr(Ide.dhChegadaDest));
  end;

  // Reforma Tritutária
  if Ide.gCompraGov.pRedutor > 0 then
  begin
    AINIRec.WriteString('ide', 'tpEnteGov', tpEnteGovToStr(Ide.gCompraGov.tpEnteGov));
    AINIRec.WriteFloat('ide', 'pRedutor', Ide.gCompraGov.pRedutor);
  end;

  if StrToModeloCTe(Ok, IntToStr(Ide.modelo)) = moCTe then
    AINIRec.WriteString('toma3', 'toma', TpTomadorToStr(Ide.toma03.Toma));
end;

procedure TCTeIniWriter.Gerar_CTe(AINIRec: TMemIniFile);
begin
  Gerar_Tomador4(AINIRec, FCTe.Ide.toma4);
  Gerar_Complemento(AINIRec, FCTe.Compl);
  Gerar_ComplementoPassagem(AINIRec, FCTe.Compl.fluxo.pass);
  Gerar_ComplementoObsCont(AINIRec, FCTe.Compl.ObsCont);
  Gerar_ComplementoObsFisco(AINIRec, FCTe.Compl.ObsFisco);
  Gerar_Emitente(AINIRec, FCTe.Emit);
  Gerar_Remetente(AINIRec, FCTe.Rem);
  Gerar_LocalColeta(AINIRec, FCTe.Rem.locColeta);
  Gerar_Expedidor(AINIRec, FCTe.exped);
  Gerar_Recebedor(AINIRec, FCTe.receb);
  Gerar_Destinatario(AINIRec, FCTe.dest);
  Gerar_ValorPrestacao(AINIRec, FCTe.vPrest);
  Gerar_Imposto(AINIRec, FCTe.Imp);

  case FCTe.ide.tpCTe of
    tcNormal:
      begin
        Gerar_ValorPrestacaoComposicao(AINIRec, FCTe.vPrest.Comp);
        Gerar_InfCTeNormal(AINIRec, FCTe.infCTeNorm);
      end;
    tcComplemento:
      begin
        Gerar_InfCTeComplemento(AINIRec, FCTe.infCteComp);
        Gerar_InfCTeComplemento10(AINIRec, FCTe.infCteComp10);
      end;
  end;

  Gerar_AutorizadosXml(AINIRec, FCTe.autXML);
  Gerar_InfRespTecnico(AINIRec, FCTe.infRespTec);
  Gerar_InfCTeSupl(AINIRec, FCTe.infCTeSupl);
end;

procedure TCTeIniWriter.Gerar_CTeSimp(AINIRec: TMemIniFile);
begin
  Gerar_Complemento(AINIRec, FCTe.Compl);
  Gerar_ComplementoPassagem(AINIRec, FCTe.Compl.fluxo.pass);
  Gerar_ComplementoObsCont(AINIRec, FCTe.Compl.ObsCont);
  Gerar_ComplementoObsFisco(AINIRec, FCTe.Compl.ObsFisco);
  Gerar_Emitente(AINIRec, FCTe.emit);
  Gerar_Tomador(AINIRec, FCTe.toma);
  Gerar_InfCarga(AINIRec, FCTe.infCarga);
  Gerar_Detalhamento(AINIRec, FCTe.det);
  Gerar_InfModalRodoviario(AINIRec, FCTe.infModal.rodo);
  Gerar_InfModalAereo(AINIRec, FCTe.infModal.aereo);
  Gerar_InfModalAquav(AINIRec, FCTe.infModal.aquav);
  Gerar_InfCTeNormalCobr(AINIRec, FCTe.cobr);
  Gerar_InfCTeNormalInfCTeSub(AINIRec, FCTe.infCteSub);
  Gerar_Imposto(AINIRec, FCTe.imp);
  Gerar_Total(AINIRec, FCTe.total);
  Gerar_AutorizadosXml(AINIRec, FCTe.autXML);
  Gerar_InfRespTecnico(AINIRec, FCTe.infRespTec);
  Gerar_InfCTeSupl(AINIRec, FCTe.infCTeSupl);
end;

procedure TCTeIniWriter.Gerar_CTeOS(AINIRec: TMemIniFile);
begin
  Gerar_InfPercurso(AINIRec, FCTe.ide.infPercurso);
  Gerar_Complemento(AINIRec, FCTe.Compl);
  Gerar_ComplementoPassagem(AINIRec, FCTe.Compl.fluxo.pass);
  Gerar_ComplementoObsCont(AINIRec, FCTe.Compl.ObsCont);
  Gerar_ComplementoObsFisco(AINIRec, FCTe.Compl.ObsFisco);
  Gerar_Emitente(AINIRec, FCTe.emit);
  Gerar_Tomador(AINIRec, FCTe.toma);
  Gerar_ValorPrestacao(AINIRec, FCTe.vPrest);
  Gerar_Imposto(AINIRec, FCTe.imp);

  FCTe.infCTeNorm.refCTeCanc := AINIRec.ReadString('infCTeNorm', 'refCTeCanc', '');

  case FCTe.ide.tpCTe of
    tcNormal:
      begin
        Gerar_InfServico(AINIRec, FCTe.infCTeNorm.infServico);
        Gerar_InfDocReferencia(AINIRec, FCTe.infCTeNorm.infDocRef);
        Gerar_Seguro(AINIRec, FCTe.infCTeNorm.seg);
        Gerar_InfModalRodoviarioOS(AINIRec, FCTe.infCTeNorm.rodoOS);
        Gerar_InfCTeSub(AINIRec, FCTe.infCTeNorm.infCteSub);
        Gerar_InfCTeNormalCobr(AINIRec, FCTe.infCTeNorm.cobr);
        Gerar_InfGTVe(AINIRec, FCTe.infCTeNorm.infGTVe);
      end;
    tcComplemento:
      begin
        Gerar_InfCTeComplemento(AINIRec, FCTe.infCteComp);
        Gerar_InfCTeComplemento10(AINIRec, FCTe.infCteComp10);
      end;
  end;

  Gerar_AutorizadosXml(AINIRec, FCTe.autXML);
  Gerar_InfRespTecnico(AINIRec, FCTe.infRespTec);
  Gerar_InfCTeSupl(AINIRec, FCTe.infCTeSupl);
end;

procedure TCTeIniWriter.Gerar_GTVe(AINIRec: TMemIniFile);
begin
  Gerar_Complemento(AINIRec, FCTe.Compl);
  Gerar_ComplementoPassagem(AINIRec, FCTe.Compl.fluxo.pass);
  Gerar_ComplementoObsCont(AINIRec, FCTe.Compl.ObsCont);
  Gerar_ComplementoObsFisco(AINIRec, FCTe.Compl.ObsFisco);
  Gerar_Emitente(AINIRec, FCTe.emit);
  Gerar_Remetente(AINIRec, FCTe.Rem);
  Gerar_Destinatario(AINIRec, FCTe.dest);
  Gerar_Origem(AINIRec, FCTe.origem);
  Gerar_Destino(AINIRec, FCTe.destino);
  Gerar_DetalhamentoGTV(AINIRec, FCTe.detGTV);
  Gerar_AutorizadosXml(AINIRec, FCTe.autXML);
  Gerar_InfRespTecnico(AINIRec, FCTe.infRespTec);
  Gerar_InfCTeSupl(AINIRec, FCTe.infCTeSupl);
end;

procedure TCTeIniWriter.Gerar_Tomador4(AINIRec: TMemIniFile; toma4: TToma4);
begin
  AINIRec.WriteString('toma4', 'CNPJCPF', toma4.CNPJCPF);
  AINIRec.WriteString('toma4', 'IE', toma4.IE);
  AINIRec.WriteString('toma4', 'xNome', toma4.xNome);
  AINIRec.WriteString('toma4', 'xFant', toma4.xFant);
  AINIRec.WriteString('toma4', 'fone', toma4.fone);

  with toma4.enderToma do
  begin
    AINIRec.WriteString('toma4', 'xLgr', xLgr);
    AINIRec.WriteString('toma4', 'nro', nro);
    AINIRec.WriteString('toma4', 'xCpl', xCpl);
    AINIRec.WriteString('toma4', 'xBairro', xBairro);
    AINIRec.WriteInteger('toma4' ,'cMun', cMun);
    AINIRec.WriteString('toma4', 'xMun', xMun);
    AINIRec.WriteInteger('toma4', 'CEP', CEP);
    AINIRec.WriteString('toma4', 'UF', UF);
    AINIRec.WriteInteger('toma4', 'cPais', cPais);
    AINIRec.WriteString('toma4', 'xPais', xPais);
  end;

  AINIRec.WriteString('toma4', 'email', toma4.email);
end;

procedure TCTeIniWriter.Gerar_Complemento(AINIRec: TMemIniFile; Compl: TCompl);
begin
  AINIRec.WriteString('compl', 'xCaracAd', compl.xCaracAd);
  AINIRec.WriteString('compl', 'xCaracSer', compl.xCaracSer);
  AINIRec.WriteString('compl', 'xEmi', compl.xEmi);

  AINIRec.WriteString('compl', 'xOrig', compl.fluxo.xOrig);
  AINIRec.WriteString('compl', 'xDest', compl.fluxo.xDest);
  AINIRec.WriteString('compl', 'xRota', compl.fluxo.xRota);

  AINIRec.WriteString('compl', 'TipoData', TpDataPeriodoToStr(compl.Entrega.TipoData));

  case compl.Entrega.TipoData of
    tdSemData:
      begin
        AINIRec.WriteString('compl', 'tpPer', TpDataPeriodoToStr(compl.Entrega.semData.tpPer));
      end;

    tdNaData, tdAteData, tdApartirData:
      begin
        AINIRec.WriteString('compl', 'tpPer', TpDataPeriodoToStr(compl.Entrega.comData.tpPer));
        AINIRec.WriteDate('compl', 'dProg', compl.Entrega.comData.dProg);
      end;

    tdNoPeriodo:
      begin
        AINIRec.WriteString('compl', 'tpPer', TpDataPeriodoToStr(compl.Entrega.noPeriodo.tpPer));
        AINIRec.WriteDateTime('compl', 'dIni', compl.Entrega.noPeriodo.dIni);
        AINIRec.WriteDateTime('compl', 'dFim', compl.Entrega.noPeriodo.dFim);
      end;
  end;

  AINIRec.WriteString('compl', 'TipoHora', TpHorarioIntervaloToStr(compl.Entrega.TipoHora));

  case compl.Entrega.TipoHora of
    thSemHorario:
      begin
        AINIRec.WriteString('compl', 'tpHor', TpHorarioIntervaloToStr(compl.Entrega.semHora.tpHor));
      end;

    thNoHorario, thAteHorario, thApartirHorario:
      begin
        AINIRec.WriteString('compl', 'tpHor', TpHorarioIntervaloToStr(compl.Entrega.comHora.tpHor));
        AINIRec.WriteTime('compl', 'hProg', compl.Entrega.comHora.hProg);
      end;

    thNoIntervalo:
      begin
        AINIRec.WriteString('compl', 'tpHor', TpHorarioIntervaloToStr(compl.Entrega.noInter.tpHor));
        AINIRec.WriteTime('compl', 'hIni', compl.Entrega.noInter.hIni);
        AINIRec.WriteTime('compl', 'hFim', compl.Entrega.noInter.hFim);
      end;
  end;

  AINIRec.WriteString('compl', 'origCalc', compl.origCalc);
  AINIRec.WriteString('compl', 'destCalc', compl.destCalc);
  AINIRec.WriteString('compl', 'xObs', compl.xObs);
end;

procedure TCTeIniWriter.Gerar_ComplementoPassagem(AINIRec: TMemIniFile;
  pass: TPassCollection);
var
  I: Integer;
begin
  for I := 0 to pass.Count - 1 do
  begin
    AINIRec.WriteString('PASS'+IntToStrZero(I+1,3), 'xPass', pass[I].xPass);
  end;
end;

procedure TCTeIniWriter.Gerar_ComplementoObsCont(AINIRec: TMemIniFile;
  ObsCont: TObsContCollection);
var
  I: Integer;
begin
  for I := 0 to ObsCont.Count - 1 do
  begin
    AINIRec.WriteString('ObsCont'+IntToStrZero(I+1,3), 'xCampo', ObsCont[I].xCampo);
    AINIRec.WriteString('ObsCont'+IntToStrZero(I+1,3), 'xTexto', ObsCont[I].xTexto);
  end;
end;

procedure TCTeIniWriter.Gerar_ComplementoObsFisco(AINIRec: TMemIniFile;
  ObsFisco: TObsFiscoCollection);
var
  I: Integer;
begin
  for I := 0 to ObsFisco.Count - 1 do
  begin
    AINIRec.WriteString('ObsFisco'+IntToStrZero(I+1,3), 'xCampo', ObsFisco[I].xCampo);
    AINIRec.WriteString('ObsFisco'+IntToStrZero(I+1,3), 'xTexto', ObsFisco[I].xTexto);
  end;
end;

procedure TCTeIniWriter.Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
begin
  AINIRec.WriteString('emit', 'CNPJ', Emit.CNPJ);
  AINIRec.WriteString('emit', 'IE', Emit.IE);
  AINIRec.WriteString('emit', 'xNome', Emit.xNome);
  AINIRec.WriteString('emit', 'xFant', Emit.xFant);
  AINIRec.WriteString('emit', 'CRT', CRTCTeToStr(Emit.CRT));

  AINIRec.WriteString('emit', 'xLgr', Emit.enderEmit.xLgr);
  AINIRec.WriteString('emit', 'nro', Emit.enderEmit.nro);
  AINIRec.WriteString('emit', 'xCpl', Emit.enderEmit.xCpl);
  AINIRec.WriteString('emit', 'xBairro', Emit.enderEmit.xBairro);
  AINIRec.WriteInteger('emit', 'cMun', Emit.enderEmit.cMun);
  AINIRec.WriteString('emit', 'xMun', Emit.enderEmit.xMun);
  AINIRec.WriteInteger('emit', 'CEP', Emit.enderEmit.CEP);
  AINIRec.WriteString('emit', 'UF', Emit.enderEmit.UF);
  AINIRec.WriteString('emit', 'fone', Emit.enderEmit.fone);
end;

procedure TCTeIniWriter.Gerar_Remetente(AINIRec: TMemIniFile; Rem: TRem);
begin
  AINIRec.WriteString('rem', 'CNPJCPF', Rem.CNPJCPF);
  AINIRec.WriteString('rem', 'IE', Rem.IE);
  AINIRec.WriteString('rem', 'xNome', Rem.xNome);
  AINIRec.WriteString('rem', 'xFant', Rem.xFant);
  AINIRec.WriteString('rem', 'fone', Rem.fone);
  AINIRec.WriteString('rem', 'Email', Rem.email);

  AINIRec.WriteString('rem', 'xLgr', Rem.enderReme.xLgr);
  AINIRec.WriteString('rem', 'nro', Rem.enderReme.nro);
  AINIRec.WriteString('rem', 'xCpl', Rem.enderReme.xCpl);
  AINIRec.WriteString('rem', 'xBairro', Rem.enderReme.xBairro);
  AINIRec.WriteInteger('rem', 'cMun', Rem.enderReme.cMun);
  AINIRec.WriteString('rem', 'xMun', Rem.enderReme.xMun);
  AINIRec.WriteInteger('rem', 'CEP', Rem.enderReme.CEP);
  AINIRec.WriteString('rem', 'UF', Rem.enderReme.UF);
  AINIRec.WriteInteger('rem', 'PaisCod', Rem.enderReme.cPais);
  AINIRec.WriteString('rem', 'Pais', Rem.enderReme.xPais);
end;

procedure TCTeIniWriter.Gerar_LocalColeta(AINIRec: TMemIniFile;
  locColeta: TLocColeta);
begin
  AINIRec.WriteString('locColeta', 'CNPJCPF', locColeta.CNPJCPF);
  AINIRec.WriteString('locColeta', 'xNome', locColeta.xNome);
  AINIRec.WriteString('locColeta', 'xLgr', locColeta.xLgr);
  AINIRec.WriteString('locColeta', 'nro', locColeta.nro);
  AINIRec.WriteString('locColeta', 'xCpl', locColeta.xCpl);
  AINIRec.WriteString('locColeta', 'xBairro', locColeta.xBairro);
  AINIRec.WriteInteger('locColeta', 'cMun', locColeta.cMun);
  AINIRec.WriteString('locColeta', 'xMun', locColeta.xMun);
  AINIRec.WriteString('locColeta', 'UF', locColeta.UF);
end;

procedure TCTeIniWriter.Gerar_Expedidor(AINIRec: TMemIniFile; exped: TExped);
begin
  AINIRec.WriteString('Exped', 'CNPJCPF', exped.CNPJCPF);
  AINIRec.WriteString('Exped', 'IE', exped.IE);
  AINIRec.WriteString('Exped', 'xNome', exped.xNome);
  AINIRec.WriteString('Exped', 'fone', exped.fone);

  AINIRec.WriteString('Exped', 'xLgr', exped.enderExped.xLgr);
  AINIRec.WriteString('Exped', 'nro', exped.enderExped.nro);
  AINIRec.WriteString('Exped', 'xCpl', exped.enderExped.xCpl);
  AINIRec.WriteString('Exped', 'xBairro', exped.enderExped.xBairro);
  AINIRec.WriteInteger('Exped', 'cMun', exped.enderExped.cMun);
  AINIRec.WriteString('Exped', 'xMun', exped.enderExped.xMun);
  AINIRec.WriteInteger('Exped', 'CEP', exped.enderExped.CEP);
  AINIRec.WriteString('Exped', 'UF', exped.enderExped.UF);
  AINIRec.WriteInteger('Exped', 'cPais', exped.enderExped.cPais);
  AINIRec.WriteString('Exped', 'xPais', exped.enderExped.xPais);
end;

procedure TCTeIniWriter.Gerar_Recebedor(AINIRec: TMemIniFile; receb: TReceb);
begin
  AINIRec.WriteString('Receb', 'CNPJCPF', receb.CNPJCPF);
  AINIRec.WriteString('Receb', 'IE', receb.IE);
  AINIRec.WriteString('Receb', 'xNome', receb.xNome);
  AINIRec.WriteString('Receb', 'fone', receb.fone);

  AINIRec.WriteString('Receb', 'xLgr', receb.enderReceb.xLgr);
  AINIRec.WriteString('Receb', 'nro', receb.enderReceb.nro);
  AINIRec.WriteString('Receb', 'xCpl', receb.enderReceb.xCpl);
  AINIRec.WriteString('Receb', 'xBairro', receb.enderReceb.xBairro);
  AINIRec.WriteInteger('Receb', 'cMun', receb.enderReceb.cMun);
  AINIRec.WriteString('Receb', 'xMun', receb.enderReceb.xMun);
  AINIRec.WriteInteger('Receb', 'CEP', receb.enderReceb.CEP);
  AINIRec.WriteString('Receb', 'UF', receb.enderReceb.UF);
  AINIRec.WriteInteger('Receb', 'cPais', receb.enderReceb.cPais);
  AINIRec.WriteString('Receb', 'xPais', receb.enderReceb.xPais);
end;

procedure TCTeIniWriter.Gerar_Destinatario(AINIRec: TMemIniFile; dest: TDest);
begin
  AINIRec.WriteString('Dest', 'CNPJCPF', Dest.CNPJCPF);
  AINIRec.WriteString('Dest', 'IE', Dest.IE);
  AINIRec.WriteString('Dest', 'xNome', Dest.xNome);
  AINIRec.WriteString('Dest', 'fone', Dest.fone);

  AINIRec.WriteString('Dest', 'xLgr', Dest.enderDest.xLgr);
  AINIRec.WriteString('Dest', 'nro', Dest.enderDest.nro);
  AINIRec.WriteString('Dest', 'xCpl', Dest.enderDest.xCpl);
  AINIRec.WriteString('Dest', 'xBairro', Dest.enderDest.xBairro);
  AINIRec.WriteInteger('Dest', 'cMun', Dest.enderDest.cMun);
  AINIRec.WriteString('Dest', 'xMun', Dest.enderDest.xMun);
  AINIRec.WriteInteger('Dest', 'CEP', Dest.enderDest.CEP);
  AINIRec.WriteString('Dest', 'UF', Dest.enderDest.UF);
  AINIRec.WriteInteger('Dest', 'cPais', Dest.enderDest.cPais);
  AINIRec.WriteString('Dest', 'xPais', Dest.enderDest.xPais);
end;

procedure TCTeIniWriter.Gerar_ValorPrestacao(AINIRec: TMemIniFile;
  vPrest: TVPrest);
begin
  AINIRec.WriteString('vPrest', 'vTPrest', CurrToStr(vPrest.vTPrest));
  AINIRec.WriteString('vPrest', 'vRec', CurrToStr(vPrest.vRec));
end;

procedure TCTeIniWriter.Gerar_ValorPrestacaoComposicao(AINIRec: TMemIniFile;
  Comp: TCompCollection);
var
  I: Integer;
  sSecao: string;
begin
  for i := 0 to comp.Count - 1 do
  begin
    sSecao := 'Comp' + IntToStrZero(I+1, 3);
    with comp.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'xNome', xNome);
      AINIRec.WriteString(sSecao, 'vComp', CurrToStr(vComp));
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_Imposto(AINIRec: TMemIniFile; Imp: TImp);
begin
  AINIRec.WriteString('Imp', 'vTotTrib', CurrToStr(Imp.vTotTrib));
  AINIRec.WriteString('Imp', 'infAdFisco', Imp.infAdFisco);
  AINIRec.WriteFloat('Imp', 'vTotDFe', Imp.vTotDFe);

  if Imp.ICMS.ICMS00.CST = cst00 then
  begin
    AINIRec.WriteString('ICMS00', 'CST', CSTICMSToStr(Imp.ICMS.ICMS00.CST));
    AINIRec.WriteString('ICMS00', 'vBC', CurrToStr(Imp.ICMS.ICMS00.vBC));
    AINIRec.WriteString('ICMS00', 'pICMS', CurrToStr(Imp.ICMS.ICMS00.pICMS));
    AINIRec.WriteString('ICMS00', 'vICMS', CurrToStr(Imp.ICMS.ICMS00.vICMS));
  end;

  if Imp.ICMS.ICMS20.CST = cst20 then
  begin
    AINIRec.WriteString('ICMS20', 'CST', CSTICMSToStr(Imp.ICMS.ICMS20.CST));
    AINIRec.WriteString('ICMS20', 'pRedBC', CurrToStr(Imp.ICMS.ICMS20.pRedBC));
    AINIRec.WriteString('ICMS20', 'vBC', CurrToStr(Imp.ICMS.ICMS20.vBC));
    AINIRec.WriteString('ICMS20', 'pICMS', CurrToStr(Imp.ICMS.ICMS20.pICMS));
    AINIRec.WriteString('ICMS20', 'vICMS', CurrToStr(Imp.ICMS.ICMS20.vICMS));
    AINIRec.WriteString('ICMS20', 'vICMSDeson', CurrToStr(Imp.ICMS.ICMS20.vICMSDeson));
    AINIRec.WriteString('ICMS20', 'cBenef', Imp.ICMS.ICMS20.cBenef);
  end;

  if Imp.ICMS.ICMS45.CST = cst45 then
  begin
    AINIRec.WriteString('ICMS45', 'CST', CSTICMSToStr(Imp.ICMS.ICMS45.CST));
    AINIRec.WriteString('ICMS45', 'vICMSDeson', CurrToStr(Imp.ICMS.ICMS45.vICMSDeson));
    AINIRec.WriteString('ICMS45', 'cBenef', Imp.ICMS.ICMS45.cBenef);
  end;

  if Imp.ICMS.ICMS60.CST = cst60 then
  begin
    AINIRec.WriteString('ICMS60', 'CST', CSTICMSToStr(Imp.ICMS.ICMS60.CST));
    AINIRec.WriteString('ICMS60', 'vBCSTRet', CurrToStr(Imp.ICMS.ICMS60.vBCSTRet));
    AINIRec.WriteString('ICMS60', 'vICMSSTRet', CurrToStr(Imp.ICMS.ICMS60.vICMSSTRet));
    AINIRec.WriteString('ICMS60', 'pICMSSTRet', CurrToStr(Imp.ICMS.ICMS60.pICMSSTRet));
    AINIRec.WriteString('ICMS60', 'vCred', CurrToStr(Imp.ICMS.ICMS60.vCred));
    AINIRec.WriteString('ICMS60', 'vICMSDeson', CurrToStr(Imp.ICMS.ICMS60.vICMSDeson));
    AINIRec.WriteString('ICMS60', 'cBenef', Imp.ICMS.ICMS60.cBenef);
  end;

  if Imp.ICMS.ICMS90.CST = cst90 then
  begin
    AINIRec.WriteString('ICMS90', 'CST', CSTICMSToStr(Imp.ICMS.ICMS90.CST));
    AINIRec.WriteString('ICMS90', 'pRedBC', CurrToStr(Imp.ICMS.ICMS90.pRedBC));
    AINIRec.WriteString('ICMS90', 'vBC', CurrToStr(Imp.ICMS.ICMS90.vBC));
    AINIRec.WriteString('ICMS90', 'pICMS', CurrToStr(Imp.ICMS.ICMS90.pICMS));
    AINIRec.WriteString('ICMS90', 'vICMS', CurrToStr(Imp.ICMS.ICMS90.vICMS));
    AINIRec.WriteString('ICMS90', 'vCred', CurrToStr(Imp.ICMS.ICMS90.vCred));
    AINIRec.WriteString('ICMS90', 'vICMSDeson', CurrToStr(Imp.ICMS.ICMS90.vICMSDeson));
    AINIRec.WriteString('ICMS90', 'cBenef', Imp.ICMS.ICMS90.cBenef);
  end;

  if Imp.ICMS.ICMSOutraUF.CST = cstICMSOutraUF then
  begin
    AINIRec.WriteString('ICMSOutraUF', 'CST', CSTICMSToStr(Imp.ICMS.ICMSOutraUF.CST));
    AINIRec.WriteString('ICMSOutraUF', 'pRedBCOutraUF', CurrToStr(Imp.ICMS.ICMSOutraUF.pRedBCOutraUF));
    AINIRec.WriteString('ICMSOutraUF', 'vBCOutraUF', CurrToStr(Imp.ICMS.ICMSOutraUF.vBCOutraUF));
    AINIRec.WriteString('ICMSOutraUF', 'pICMSOutraUF', CurrToStr(Imp.ICMS.ICMSOutraUF.pICMSOutraUF));
    AINIRec.WriteString('ICMSOutraUF', 'vICMSOutraUF', CurrToStr(Imp.ICMS.ICMSOutraUF.vICMSOutraUF));
    AINIRec.WriteString('ICMSOutraUF', 'vICMSDeson', CurrToStr(Imp.ICMS.ICMSOutraUF.vICMSDeson));
    AINIRec.WriteString('ICMSOutraUF', 'cBenef', Imp.ICMS.ICMSOutraUF.cBenef);
  end;

  {indica se é simples}
  if (Imp.ICMS.ICMSSN.indSN = 1) and (Imp.ICMS.SituTrib = cstICMSSN) then
    AINIRec.WriteInteger('ICMSSN', 'indSN', Imp.ICMS.ICMSSN.indSN);

  AINIRec.WriteFloat('ICMSUFFim', 'vBCUFFim', Imp.ICMSUFFim.vBCUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'pFCPUFFim', Imp.ICMSUFFim.pFCPUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'pICMSUFFim', Imp.ICMSUFFim.pICMSUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'pICMSInter', Imp.ICMSUFFim.pICMSInter);
  AINIRec.WriteFloat('ICMSUFFim', 'pICMSInterPart', Imp.ICMSUFFim.pICMSInterPart);
  AINIRec.WriteFloat('ICMSUFFim', 'vFCPUFFim', Imp.ICMSUFFim.vFCPUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'vICMSUFFim', Imp.ICMSUFFim.vICMSUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'vICMSUFIni', Imp.ICMSUFFim.vICMSUFIni);

  //CT-e OS
  if FCTe.ide.modelo = 67 then
  begin
    AINIRec.WriteString('infTribFed', 'vPIS', CurrToStr(Imp.infTribFed.vPIS));
    AINIRec.WriteString('infTribFed', 'vCOFINS', CurrToStr(Imp.infTribFed.vCOFINS));
    AINIRec.WriteString('infTribFed', 'vIR', CurrToStr(Imp.infTribFed.vIR));
    AINIRec.WriteString('infTribFed', 'vINSS', CurrToStr(Imp.infTribFed.vINSS));
    AINIRec.WriteString('infTribFed', 'vCSLL', CurrToStr(Imp.infTribFed.vCSLL));
  end;

  // Reforma Tributária
  Gerar_IBSCBS(AINIRec, CTe.imp.IBSCBS);
end;

procedure TCTeIniWriter.Gerar_InfCTeNormal(AINIRec: TMemIniFile;
  infCTeNorm: TInfCTeNorm);
begin
  Gerar_InfCarga(AINIRec, infCTeNorm.infCarga);

  Gerar_InfCTeNormalInfNF(AINIRec, infCTeNorm.infDoc.infNF);
  Gerar_InfCTeNormalInfNFe(AINIRec, infCTeNorm.infDoc.infNFe);
  Gerar_InfCTeNormalInfOutros(AINIRec, infCTeNorm.infDoc.infOutros);
  Gerar_InfCTeNormalInfDCe(AINIRec, infCTeNorm.infDoc.infDCe);

  Gerar_InfCTeNormalDocAnteriores(AINIRec, infCTeNorm.docAnt);
  Gerar_InfCTeNormalInfModal(AINIRec, infCTeNorm);
  Gerar_InfCTeNormalVeiculosNovos(AINIRec, infCTeNorm.veicNovos);

  if not (FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl]) then
  begin
    Gerar_InfCTeNormalCobr(AINIRec, infCTeNorm.cobr);
    Gerar_InfCTeNormalInfCTeSub(AINIRec, infCTeNorm.infCteSub);
  end;

  Gerar_InfCTeNormalInfGlobalizado(AINIRec, infCTeNorm.infGlobalizado);
  Gerar_InfCTeNormalInfCTeMultimodal(AINIRec, infCTeNorm.infServVinc);
end;

procedure TCTeIniWriter.Gerar_InfCarga(AINIRec: TMemIniFile;
  infCarga: TInfCarga);
var
  i: Integer;
  sSecao: string;
begin
  AINIRec.WriteString('infCarga', 'vCarga', CurrToStr(infCarga.vCarga));
  AINIRec.WriteString('infCarga', 'proPred', infCarga.proPred);
  AINIRec.WriteString('infCarga', 'xOutCat', infCarga.xOutCat);
  AINIRec.WriteString('infCarga', 'vCargaAverb', CurrToStr(infCarga.vCargaAverb));

  for i := 0 to infCarga.infQ.Count -1 do
  begin
    sSecao := 'infQ' + IntToStrZero(I+1, 3);

    with infCarga.infQ.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'cUnid', UnidMedToStr(cUnid));
      AINIRec.WriteString(sSecao, 'tpMed', tpMed);
      AINIRec.WriteString(sSecao, 'qCarga', CurrToStr(qCarga));
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalInfNF(AINIRec: TMemIniFile;
  infNF: TInfNFCollection);
var
  i, j, k, l: Integer;
  sSecao: string;
begin
  for i := 0 to infNF.Count -1 do
  begin
    sSecao := 'infNF' + IntToStrZero(I+1, 3);

    with infNF.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'nRoma', nRoma);
      AINIRec.WriteString(sSecao, 'nPed', nPed);
      AINIRec.WriteString(sSecao, 'mod', ModeloNFToStrEX(modelo));
      AINIRec.WriteString(sSecao, 'serie', serie);
      AINIRec.WriteString(sSecao, 'nDoc', nDoc);
      AINIRec.WriteString(sSecao, 'dEmi', DateToStr(dEmi));
      AINIRec.WriteString(sSecao, 'vBC', CurrToStr(vBC));
      AINIRec.WriteString(sSecao, 'vICMS', CurrToStr(vICMS));
      AINIRec.WriteString(sSecao, 'vBCST', CurrToStr(vBCST));
      AINIRec.WriteString(sSecao, 'vST', CurrToStr(vST));
      AINIRec.WriteString(sSecao, 'vProd', CurrToStr(vProd));
      AINIRec.WriteString(sSecao, 'vNF', CurrToStr(vNF));
      AINIRec.WriteInteger(sSecao, 'nCFOP', nCFOP);
      AINIRec.WriteString(sSecao, 'nPeso', CurrToStr(nPeso));
      AINIRec.WriteString(sSecao, 'PIN', PIN);

      for j := 0 to infUnidCarga.Count - 1 do
      begin
        sSecao := 'infUnidCarga'+ IntToStrZero(I+1, 3) + IntToStrZero(J+1,3);

        if AINIRec.SectionExists(sSecao) then
          continue;

        with infUnidCarga.Items[j] do
        begin
          AINIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));
          AINIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
          AINIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);

          for k:= 0 to lacUnidCarga.Count - 1 do
            AINIRec.WriteString('lacUnidCarga'+IntToStrZero(I+1, 3)+IntToStrZero(J+1, 3)+IntToStrZero(K+1, 3)
                               , 'nLacre'
                               , lacUnidCarga[k].nLacre);

        end;
      end;

      for j := 0 to infUnidTransp.Count - 1 do
      begin
        sSecao := 'infUnidTransp' + IntToStrZero(I+1,3) + IntToStrZero(J+1,3);

        if AINIRec.SectionExists(sSecao) then
          continue;

        with infUnidTransp.Items[j] do
        begin
          AINIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(tpUnidTransp));
          AINIRec.WriteString(sSecao, 'idUnidTransp', idUnidTransp);
          AINIRec.WriteFloat(sSecao, 'qtdRat'       , qtdRat);

          for k := 0 to lacUnidTransp.Count - 1 do
            AINIRec.WriteString('lacUnidTransp'+IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3)
                               , 'nLacre'
                               , lacUnidTransp[k].nLacre);

          for k := 0 to infUnidCarga.Count - 1 do
          begin
            sSecao := 'infUnidCarga'+ IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3);

            with infUnidCarga.Items[k] do
            begin
              AINIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));
              AINIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
              AINIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);

              for l := 0 to lacUnidCarga.Count - 1 do
                AINIRec.WriteString('lacUnidCarga'+IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3) + IntToStrZero(L+1, 3)
                                   , 'nLacre'
                                   , lacUnidCarga[l].nLacre);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalInfNFe(AINIRec: TMemIniFile;
  infNFe: TInfNFeCollection);
var
  i, j, k, l: Integer;
  sSecao: string;
begin
  for i := 0 to infNFe.Count -1 do
  begin
    sSecao := 'infNFe' + IntToStrZero(I+1, 3);

    with infNFe.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'chave', chave);
      AINIRec.WriteString(sSecao, 'PIN', PIN);
      AINIRec.WriteDate(sSecao, 'dPrev', dPrev);

      for j := 0 to infUnidCarga.Count - 1 do
      begin
        sSecao := 'infUnidCarga'+ IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3);

        if AINIRec.SectionExists(sSecao) then
          continue;

        with infUnidCarga.Items[j] do
        begin
          AINIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));
          AINIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
          AINIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);

          for k := 0 to lacUnidCarga.Count - 1 do
            AINIRec.WriteString('lacUnidCarga'+IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3)
                               , 'nLacre'
                               , lacUnidCarga[k].nLacre);
        end;
      end;

      for j := 0 to infUnidTransp.Count - 1 do
      begin
        sSecao := 'infUnidTransp' + IntToStrZero(I+1,3) + IntToStrZero(J+1,3);

        if AINIRec.SectionExists(sSecao) then
          continue;

        with infUnidTransp.Items[j] do
        begin
          AINIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(tpUnidTransp));
          AINIRec.WriteString(sSecao, 'idUnidTransp', idUnidTransp);
          AINIRec.WriteFloat(sSecao, 'qtdRat'       , qtdRat);

          for k := 0 to lacUnidTransp.Count - 1 do
            AINIRec.WriteString('lacUnidTransp'+IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3)
                               , 'nLacre'
                               , lacUnidTransp[k].nLacre);

          for k := 0 to infUnidCarga.Count - 1 do
          begin
            sSecao := 'infUnidCarga'+ IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3);

            with infUnidCarga.Items[k] do
            begin
              AINIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));
              AINIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
              AINIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);

              for l := 0 to lacUnidCarga.Count - 1 do
                AINIRec.WriteString('lacUnidCarga'+IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3) + IntToStrZero(L+1, 3)
                                   , 'nLacre'
                                   , lacUnidCarga[l].nLacre);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalInfOutros(AINIRec: TMemIniFile;
  infOutros: TInfOutrosCollection);
var
  i, j, k, l: Integer;
  sSecao: string;
begin
  for i:= 0 to infOutros.Count - 1 do
  begin
    sSecao := 'infOutros' + IntToStrZero(i+1, 3);

    with infOutros[i] do
    begin
      AINIRec.WriteString(sSecao, 'tpDoc', TpDocumentoToStr(tpDoc));
      AINIRec.WriteString(sSecao, 'descOutros', descOutros);
      AINIRec.WriteString(sSecao, 'nDoc', nDoc);
      AINIRec.WriteDate(sSecao, 'dEmi', dEmi);
      AINIRec.WriteFloat(sSecao, 'vDocFisc', vDocFisc);
      AINIRec.WriteDate(sSecao, 'dPrev', dPrev);

      for j := 0 to infUnidCarga.Count - 1 do
      begin
        sSecao := 'infUnidCarga'+ IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3);

        if AINIRec.SectionExists(sSecao) then
          continue;

        with infUnidCarga.Items[j] do
        begin
          AINIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));
          AINIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
          AINIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);

          for k := 0 to lacUnidCarga.Count - 1 do
            AINIRec.WriteString('lacUnidCarga'+IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3)
                               , 'nLacre'
                               , lacUnidCarga[k].nLacre);
        end;
      end;

      for j := 0 to infUnidTransp.Count - 1 do
      begin
        sSecao := 'infUnidTransp' + IntToStrZero(I+1,3) + IntToStrZero(J+1,3);

        if AINIRec.SectionExists(sSecao) then
          continue;

        with infUnidTransp.Items[j] do
        begin
          AINIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(tpUnidTransp));
          AINIRec.WriteString(sSecao, 'idUnidTransp', idUnidTransp);
          AINIRec.WriteFloat(sSecao, 'qtdRat'       , qtdRat);

          for k := 0 to lacUnidTransp.Count - 1 do
            AINIRec.WriteString('lacUnidTransp'+IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3)
                               , 'nLacre'
                               , lacUnidTransp[k].nLacre);

          for k := 0 to infUnidCarga.Count - 1 do
          begin
            sSecao := 'infUnidCarga'+ IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3);

            with infUnidCarga.Items[k] do
            begin
              AINIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));
              AINIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
              AINIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);

              for l := 0 to lacUnidCarga.Count - 1 do
                AINIRec.WriteString('lacUnidCarga'+IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3) + IntToStrZero(K+1, 3) + IntToStrZero(L+1, 3)
                                   , 'nLacre'
                                   , lacUnidCarga[l].nLacre);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalInfDCe(AINIRec: TMemIniFile;
  infDCe: TInfDCeCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infDCe.Count -1 do
  begin
    sSecao := 'infDCe' + IntToStrZero(I+1, 3);

    with infDCe.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'chave', chave);
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalDocAnteriores(AINIRec: TMemIniFile;
  docAnt: TDocAnt);
var
  i, j, k: Integer;
  sSecao: string;
begin
  for i := 0 to docAnt.emiDocAnt.Count - 1 do
  begin
    sSecao := 'emiDocAnt'+ IntToStrZero(I+1, 3);

    AINIRec.WriteString(sSecao, 'CNPJCPF', docAnt.emiDocAnt[i].CNPJCPF);
    AINIRec.WriteString(sSecao, 'IE', docAnt.emiDocAnt[i].IE);
    AINIRec.WriteString(sSecao, 'UF', docAnt.emiDocAnt[i].UF);
    AINIRec.WriteString(sSecao, 'xNome', docAnt.emiDocAnt[i].xNome);

    for j := 0 to docAnt.emiDocAnt[i].idDocAnt.Count - 1 do
    begin
      for k := 0 to docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap.Count - 1 do
      begin
        sSecao := 'idDocAntPa'+ IntToStrZero(I+1, 3) + IntToStrZero(K+1, 3);

        AINIRec.WriteString(sSecao, 'tpDoc', TpDocumentoAnteriorToStr(docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].tpDoc));
        AINIRec.WriteString(sSecao, 'serie', docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].serie);
        AINIRec.WriteString(sSecao, 'subser', docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].subser);
        AINIRec.WriteString(sSecao, 'nDoc', docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].nDoc);
        AINIRec.WriteDate(sSecao, 'dEmi', docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].dEmi);
      end;

      for k := 0 to docAnt.emiDocAnt[i].idDocAnt[j].idDocAntEle.Count - 1 do
      begin
        sSecao := 'idDocAntEle' + IntToStrZero(I+1,3) + IntToStrZero(K+1, 3);

        AINIRec.WriteString(sSecao, 'chCTe', docAnt.emiDocAnt[i].idDocAnt[j].idDocAntEle[k].chCTe);
      end;
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalInfModal(AINIRec: TMemIniFile;
  infCTeNorm: TInfCTeNorm);
begin
  if FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
  begin
    case FCTe.ide.modal of
      mdRodoviario:
        Gerar_InfModalRodoviario(AINIRec, FCTe.infModal.rodo);
      mdAereo:
        Gerar_InfModalAereo(AINIRec, FCTe.infModal.aereo);
      mdAquaviario:
        Gerar_InfModalAquav(AINIRec, FCTe.infModal.aquav);
    end;
  end
  else
  begin
    case FCTe.ide.modal of
      mdRodoviario:
        Gerar_InfModalRodoviario(AINIRec, infCTeNorm.rodo);
      mdAereo:
        Gerar_InfModalAereo(AINIRec, infCTeNorm.aereo);
      mdAquaviario:
        Gerar_InfModalAquav(AINIRec, infCTeNorm.aquav);
      mdFerroviario:
        Gerar_InfModalFerrov(AINIRec, infCTeNorm.ferrov);
      mdDutoviario:
        Gerar_InfModalDuto(AINIRec, infCTeNorm.duto);
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfModalRodoviario(AINIRec: TMemIniFile;
  rodo: TRodo);
var
  I: Integer;
  sSecao: string;
begin
  if Rodo.RNTRC <> '' then
  begin
    AINIRec.WriteString('Rodo', 'RNTRC', Rodo.RNTRC);
    AINIRec.WriteString('Rodo', 'dPrev', DateToStr(Rodo.dPrev));
    AINIRec.WriteString('Rodo', 'lota', TpLotacaoToStr(Rodo.Lota));
  end;

  for i := 0 to Rodo.Occ.Count -1 do
  begin
    sSecao := 'occ' + IntToStrZero(I+1, 3);

    with Rodo.Occ.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'serie', serie);
      AINIRec.WriteInteger(sSecao, 'nOcc', nOcc);
      AINIRec.WriteString(sSecao, 'dEmi', DateToStr(dEmi));
      AINIRec.WriteString(sSecao, 'CNPJ', emiOcc.CNPJ);
      AINIRec.WriteString(sSecao, 'cInt', emiOcc.cInt);
      AINIRec.WriteString(sSecao, 'IE', emiOcc.IE);
      AINIRec.WriteString(sSecao, 'UF', emiOcc.UF);
      AINIRec.WriteString(sSecao, 'fone', emiOcc.fone);
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfModalAereo(AINIRec: TMemIniFile;
  aereo: TAereo);
var
  i: Integer;
  sSecao, Key: string;
begin
  sSecao := 'aereo';

  AINIRec.WriteInteger(sSecao, 'nMinu', aereo.nMinu);
  AINIRec.WriteString(sSecao, 'nOCA', aereo.nOCA);
  AINIRec.WriteString(sSecao, 'dPrevAereo', DateToStr(aereo.dPrevAereo));

  if FCTe.infCTe.versao = 2 then
  begin
    AINIRec.WriteString(sSecao, 'xLAgEmi', aereo.xLAgEmi);
    AINIRec.WriteString(sSecao, 'IdT', aereo.IdT);
  end;

  AINIRec.WriteString(sSecao, 'CL', aereo.tarifa.CL);
  AINIRec.WriteString(sSecao, 'cTar', aereo.tarifa.cTar);
  AINIRec.WriteFloat(sSecao, 'vTar', aereo.tarifa.vTar);

  AINIRec.WriteString(sSecao, 'xDime', aereo.natCarga.xDime);
  AINIRec.WriteString(sSecao, 'cIMP', aereo.natCarga.cIMP);

  for i := 0 to aereo.natCarga.cinfManu.Count - 1 do
  begin
    Key := 'cInfManu' + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, Key, TpInfManuToStrV2(aereo.natCarga.cinfManu[i].nInfManu));
  end;

  if FCTe.infCTe.versao >= 3 then
  begin
    Gerar_Peri(AINIRec, aereo.peri);
  end;
end;

procedure TCTeIniWriter.Gerar_Peri(AINIRec: TMemIniFile; peri: TPeriCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to peri.Count - 1 do
  begin
    sSecao := 'peri' + IntToStrZero(i + 1, 3);

    if FCTe.infCTe.versao >= 3 then
    begin
      AINIRec.WriteString(sSecao, 'nONU', peri[i].nONU);
      AINIRec.WriteString(sSecao, 'qTotEmb', peri[i].qTotEmb);

      Gerar_infTotAP(AINIRec, peri, i + 1);
    end
    else
    begin
      AINIRec.WriteString(sSecao, 'nONU', peri[i].nONU);
      AINIRec.WriteString(sSecao, 'xNomeAE', peri[i].xNomeAE);
      AINIRec.WriteString(sSecao, 'xClaRisco', peri[i].xClaRisco);
      AINIRec.WriteString(sSecao, 'grEmb', peri[i].grEmb);
      AINIRec.WriteString(sSecao, 'qTotProd', peri[i].qTotProd);
      AINIRec.WriteString(sSecao, 'qVolTipo', peri[i].qVolTipo);
      AINIRec.WriteString(sSecao, 'pontoFulgor', peri[i].pontoFulgor);
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_infTotAP(AINIRec: TMemIniFile;
  peri: TPeriCollection; Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'infTotAP' + IntToStrZero(Idx, 3);

  AINIRec.WriteString(sSecao, 'qTotProd', peri[Idx].qTotProd);
  AINIRec.WriteString(sSecao, 'uniAP', UniMedToStr(peri[Idx].uniAP));
end;

procedure TCTeIniWriter.Gerar_InfModalAquav(AINIRec: TMemIniFile;
  aquav: TAquav);
var
  I: Integer;
  sSecao: string;
begin
  sSecao := 'aquav';

  AINIRec.WriteFloat(sSecao, 'vPrest', Aquav.vPrest);
  AINIRec.WriteFloat(sSecao, 'vAFRMM', Aquav.vAFRMM);
  AINIRec.WriteString(sSecao, 'nBooking', Aquav.nBooking);
  AINIRec.WriteString(sSecao, 'nCtrl', Aquav.nCtrl);
  AINIRec.WriteString(sSecao, 'xNavio', Aquav.xNavio);
  AINIRec.WriteString(sSecao, 'nViag', Aquav.nViag);
  AINIRec.WriteString(sSecao, 'direc', TpDirecaoToStr(Aquav.direc));
  AINIRec.WriteString(sSecao, 'prtEmb', Aquav.prtEmb);
  AINIRec.WriteString(sSecao, 'prtTrans', Aquav.prtTrans);
  AINIRec.WriteString(sSecao, 'prtDest', Aquav.prtDest);
  AINIRec.WriteString(sSecao, 'tpNav', TpNavegacaoToStr(Aquav.tpNav));
  AINIRec.WriteString(sSecao, 'irin', Aquav.irin);

  for i := 0 to Aquav.balsa.Count - 1 do
  begin
    sSecao := 'balsa'+IntToStrZero(I,3);

    AINIRec.WriteString(sSecao, 'xBalsa', Aquav.balsa[i].xBalsa);
  end;
end;

procedure TCTeIniWriter.Gerar_InfModalFerrov(AINIRec: TMemIniFile;
  ferrov: TFerrov);
var
  I: Integer;
  sSecao: string;
begin
  sSecao := 'ferrov';

  AINIRec.WriteString(sSecao, 'tpTraf', TpTrafegoToStr(Ferrov.tpTraf));
  AINIRec.WriteString(sSecao, 'fluxo', Ferrov.fluxo);
  AINIRec.WriteFloat(sSecao, 'vFrete', Ferrov.vFrete);

  AINIRec.WriteString(sSecao, 'respFat', TrafegoMutuoToStr(Ferrov.trafMut.respFat));
  AINIRec.WriteString(sSecao, 'ferrEmi', TrafegoMutuoToStr(Ferrov.trafMut.ferrEmi));

  if FCTe.infCTe.versao >= 3 then
    AINIRec.WriteString(sSecao, 'chCTeFerroOrigem', Ferrov.trafMut.chCTeFerroOrigem)
  else
    AINIRec.WriteString(sSecao, 'idTrem', Ferrov.idTrem);

  for i := 0 to Ferrov.ferroEnv.Count - 1 do
  begin
    sSecao := 'ferroEnv'+IntToStrZero(I,3);

    AINIRec.WriteString(sSecao, 'CNPJ', Ferrov.ferroEnv[i].CNPJ);
    AINIRec.WriteString(sSecao, 'IE', Ferrov.ferroEnv[i].IE);
    AINIRec.WriteString(sSecao, 'xNome', Ferrov.ferroEnv[i].xNome);

    AINIRec.WriteString(sSecao, 'xLgr', Ferrov.ferroEnv[i].enderFerro.xLgr);
    AINIRec.WriteString(sSecao, 'nro', Ferrov.ferroEnv[i].enderFerro.nro);
    AINIRec.WriteString(sSecao, 'xCpl', Ferrov.ferroEnv[i].enderFerro.xCpl);
    AINIRec.WriteString(sSecao, 'xBairro', Ferrov.ferroEnv[i].enderFerro.xBairro);
    AINIRec.WriteInteger(sSecao, 'cMun', Ferrov.ferroEnv[i].enderFerro.cMun);
    AINIRec.WriteString(sSecao, 'xMun', Ferrov.ferroEnv[i].enderFerro.xMun);
    AINIRec.WriteInteger(sSecao, 'CEP', Ferrov.ferroEnv[i].enderFerro.CEP);
    AINIRec.WriteString(sSecao, 'UF', Ferrov.ferroEnv[i].enderFerro.UF);
  end;

  if FCTe.infCTe.versao < 3 then
  begin
    for i := 0 to Ferrov.detVag.Count - 1 do
    begin
      sSecao := 'detVag'+IntToStrZero(I,3);

      AINIRec.WriteInteger(sSecao, 'nVag', Ferrov.detVag[i].nVag);
      AINIRec.WriteFloat(sSecao, 'cap', Ferrov.detVag[i].cap);
      AINIRec.WriteString(sSecao, 'tpVag', Ferrov.detVag[i].tpVag);
      AINIRec.WriteFloat(sSecao, 'pesoR', Ferrov.detVag[i].pesoR);
      AINIRec.WriteFloat(sSecao, 'pesoBC', Ferrov.detVag[i].pesoBC);
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfModalDuto(AINIRec: TMemIniFile; duto: TDuto);
var
  sSecao: string;
begin
  sSecao := 'duto';

  AINIRec.WriteFloat(sSecao, 'vTar', duto.vTar);
  AINIRec.WriteString(sSecao, 'dIni', DateToStr(duto.dIni));
  AINIRec.WriteString(sSecao, 'dFim', DateToStr(duto.dFim));
  AINIRec.WriteString(sSecao, 'classDuto', classDutoToStr(duto.classDuto));
  AINIRec.WriteString(sSecao, 'tpContratacao', tpContratacaoToStr(duto.tpContratacao));
  AINIRec.WriteString(sSecao, 'codPontoEntrada', duto.codPontoEntrada);
  AINIRec.WriteString(sSecao, 'codPontoSaida', duto.codPontoSaida);
  AINIRec.WriteString(sSecao, 'nContrato', duto.nContrato);
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalVeiculosNovos(AINIRec: TMemIniFile;
  veicNovos: TVeicNovosCollection);
var
  I: Integer;
  sSecao: string;
begin
  for i:= 0 to veicNovos.Count - 1 do
  begin
    sSecao := 'veicNovos' + IntToStrZero(I+1, 3);

    with veicNovos.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'chassi', chassi);
      AINIRec.WriteString(sSecao, 'cCor', cCor);
      AINIRec.WriteString(sSecao, 'xCor', xCor);
      AINIRec.WriteString(sSecao, 'cMod', cMod);
      AINIRec.WriteString(sSecao, 'vUnit', CurrToStr(vUnit));
      AINIRec.WriteString(sSecao, 'vFrete', CurrToStr(vFrete));
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalCobr(AINIRec: TMemIniFile;
  cobr: TCobr);
var
  i: Integer;
  sSecao: string;
begin
  AINIRec.WriteString('cobr', 'nFat', cobr.fat.nFat);
  AINIRec.WriteString('cobr', 'vOrig', CurrToStr(cobr.fat.vOrig));
  AINIRec.WriteString('cobr', 'vDesc', CurrToStr(cobr.fat.vDesc));
  AINIRec.WriteString('cobr', 'vLiq', CurrToStr(cobr.fat.vLiq));

  for i := 0 to Cobr.Dup.Count -1 do
  begin
    sSecao := 'dup' + IntToStrZero(I+1, 3);

    with Cobr.Dup[i] do
    begin
      AINIRec.WriteString(sSecao, 'nDup', nDup);
      AINIRec.WriteString(sSecao, 'dVenc', DateToStr(dVenc));
      AINIRec.WriteString(sSecao, 'vDup', CurrToStr(vDup));
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalInfCTeSub(AINIRec: TMemIniFile;
  infCteSub: TInfCteSub);
var
  sSecao: string;
begin
  if infCteSub.chCte <> '' then
  begin
    sSecao := 'infCTeSub';

    with infCTeSub do
    begin
      AINIRec.WriteString(sSecao, 'chCTe', chCte);
      AINIRec.WriteString(sSecao, 'indAlteraToma', TIndicadorToStr(indAlteraToma));

      if FCTe.infCTe.versao = 3 then
      begin
        sSecao := 'tomaICMS';

        AINIRec.WriteString(sSecao, 'refNFe', tomaICMS.refNFe);
        AINIRec.WriteString(sSecao, 'CNPJ', tomaICMS.refNF.CNPJCPF);
        AINIRec.WriteString(sSecao, 'mod', tomaICMS.refNF.modelo);
        AINIRec.WriteInteger(sSecao, 'serie', tomaICMS.refNF.serie);
        AINIRec.WriteInteger(sSecao, 'subserie', tomaICMS.refNF.subserie);
        AINIRec.WriteInteger(sSecao, 'nro', tomaICMS.refNF.nro);
        AINIRec.WriteFloat(sSecao, 'valor', tomaICMS.refNF.valor);
        AINIRec.WriteDateTime(sSecao, 'dEmi', tomaICMS.refNF.dEmi);
        AINIRec.WriteString(sSecao, 'refCte', tomaICMS.refCte);
      end;
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalInfGlobalizado(AINIRec: TMemIniFile;
  infGlobalizado: TInfGlobalizado);
var
  sSecao: string;
begin
  sSecao := 'infGlobalizado';

  AINIRec.WriteString(sSecao, 'xObs', infGlobalizado.xObs);
end;

procedure TCTeIniWriter.Gerar_InfCTeNormalInfCTeMultimodal(AINIRec: TMemIniFile;
  infServVinc: TInfServVinc);
var
  I: Integer;
begin
  for i := 0 to infServVinc.infCTeMultimodal.Count - 1 do
    AINIRec.WriteString('infCTeMultimodal' + IntToStrZero(i+1 , 3), 'chCTeMultimodal', infServVinc.infCTeMultimodal[i].chCTeMultimodal);
end;

procedure TCTeIniWriter.Gerar_InfCTeComplemento(AINIRec: TMemIniFile;
  infCteComp: TInfCteComp);
var
  sSecao: string;
begin
  sSecao := 'infCteComp01';

  AINIRec.WriteString(sSecao, 'chave', infCteComp.chave);
end;

procedure TCTeIniWriter.Gerar_InfCTeComplemento10(AINIRec: TMemIniFile;
  infCteComp10: TInfCteCompCollection);
var
  I: Integer;
  sSecao: string;
begin
  for i := 0 to infCteComp10.Count - 1 do
  begin
    sSecao := 'InfCteComp' + IntToStrZero(i, 2);

    AINIRec.WriteString(sSecao, 'chCTe', infCteComp10[i].chCTe);
  end;
end;

procedure TCTeIniWriter.Gerar_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  I: Integer;
  sSecao: string;
begin
  for i := 0 to autXML.Count - 1 do
  begin
    sSecao := 'autXML' + IntToStrZero(I+1, 2);

    with autXML.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfRespTecnico(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
begin
  AINIRec.WriteString('infRespTec', 'CNPJ', infRespTec.CNPJ);
  AINIRec.WriteString('infRespTec', 'xContato', infRespTec.xContato);
  AINIRec.WriteString('infRespTec', 'email', infRespTec.email);
  AINIRec.WriteString('infRespTec', 'fone', infRespTec.fone);
end;

procedure TCTeIniWriter.Gerar_InfCTeSupl(AINIRec: TMemIniFile;
  infCTeSupl: TinfCTeSupl);
var
  sSecao: string;
begin
  sSecao := 'infCTeSupl';

  AINIRec.WriteString(sSecao, 'qrCodCTe', infCTeSupl.qrCodCTe);
end;

procedure TCTeIniWriter.Gerar_Tomador(AINIRec: TMemIniFile; toma: TToma);
begin
  AINIRec.WriteString('toma', 'CNPJCPF', toma.CNPJCPF);
  AINIRec.WriteString('toma', 'IE', toma.IE);
  AINIRec.WriteString('toma', 'xNome', toma.xNome);
  AINIRec.WriteString('toma', 'xFant', toma.xFant);
  AINIRec.WriteString('toma', 'fone', toma.fone);

  with toma.enderToma do
  begin
    AINIRec.WriteString('toma', 'xLgr', xLgr);
    AINIRec.WriteString('toma', 'nro', nro);
    AINIRec.WriteString('toma', 'xCpl', xCpl);
    AINIRec.WriteString('toma', 'xBairro', xBairro);
    AINIRec.WriteInteger('toma' ,'cMun', cMun);
    AINIRec.WriteString('toma', 'xMun', xMun);
    AINIRec.WriteInteger('toma', 'CEP', CEP);
    AINIRec.WriteString('toma', 'UF', UF);
    AINIRec.WriteInteger('toma', 'cPais', cPais);
    AINIRec.WriteString('toma', 'xPais', xPais);
  end;
end;

procedure TCTeIniWriter.Gerar_Detalhamento(AINIRec: TMemIniFile;
  det: TdetCollection);
var
  I, J, K: Integer;
  sSecao: string;
begin
  if FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
  begin
    for i := 0 to det.Count - 1 do
    begin
      sSecao := 'det' + IntToStrZero(I+1, 3);

      with det[i] do
      begin
        AINIRec.WriteInteger(sSecao, 'cMunIni', cMunIni);
        AINIRec.WriteString(sSecao, 'xMunIni', xMunIni);
        AINIRec.WriteInteger(sSecao, 'cMunFim', cMunFim);
        AINIRec.WriteString(sSecao, 'xMunFim', xMunFim);
        AINIRec.WriteFloat(sSecao, 'vPrest', vPrest);
        AINIRec.WriteFloat(sSecao, 'vRec', vRec);

        for j := 0 to det[i].Comp.Count - 1 do
        begin
          sSecao := 'Comp' + IntToStrZero(I+1, 3) + IntToStrZero(j+1, 3);

          with det[i].Comp[j] do
          begin
            AINIRec.WriteString(sSecao, 'xNome', xNome);
            AINIRec.WriteFloat(sSecao, 'vComp', vComp);
          end;
        end;

        for j := 0 to det[i].infNFe.Count - 1 do
        begin
          sSecao := 'infNFe' + IntToStrZero(I+1, 3) + IntToStrZero(j+1, 3);

          Gerar_Secao_InfNFe(AINIRec, infNFe, sSecao, I+1, j+1);
        end;

        for j := 0 to det[i].infDocAnt.Count - 1 do
        begin
          sSecao := 'infDocAnt' + IntToStrZero(I+1, 3) + IntToStrZero(j+1, 3);

          with det[i].infDocAnt[j] do
          begin
            AINIRec.WriteString(sSecao, 'chCTe', chCTe);
            AINIRec.WriteString(sSecao, 'tpPrest', tpPrestToStr(tpPrest));

            for k := 0 to det[i].infDocAnt[j].infNFeTranspParcial.Count - 1 do
            begin
              sSecao := 'infDocAnt' + IntToStrZero(I+1, 3) + IntToStrZero(j+1, 3) +
                                      IntToStrZero(k+1, 3);

              with det[i].infDocAnt[j].infNFeTranspParcial[j] do
              begin
                AINIRec.WriteString(sSecao, 'chNFe', chNFe);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_Secao_InfNFe(AINIRec: TMemIniFile;
  infNFe: TInfNFeCollection; const Secao: string; Idx1, Idx2: Integer);
var
  Nivel, sSecao: string;
  J, K, L: Integer;
begin
  AINIRec.WriteString(Secao, 'chave', infNFe[Idx2-1].chave);
  AINIRec.WriteString(Secao, 'PIN', infNFe[Idx2-1].PIN);
  AINIRec.WriteString(Secao, 'dPrev', DateToStr(infNFe[Idx2-1].dPrev));

  if Idx2 = -1 then
    Nivel := IntToStrZero(Idx2, 3)
  else
    Nivel := IntToStrZero(Idx1, 3) + IntToStrZero(Idx2, 3);

  for j := 0 to infNFe[Idx2-1].infUnidTransp.Count - 1 do
  begin
    sSecao := 'infUnidTransp' + Nivel + IntToStrZero(J+1,3);

    with infNFe[Idx2-1].infUnidTransp[j] do
    begin
      AINIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(tpUnidTransp));
      AINIRec.WriteString(sSecao, 'idUnidTransp', idUnidTransp);
      AINIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);

      for k := 0 to lacUnidTransp.Count - 1 do
        AINIRec.WriteString('lacUnidTransp'+ Nivel + IntToStrZero(J+1,3) + IntToStrZero(K+1, 3)
                           , 'nLacre'
                           , lacUnidTransp[k].nLacre);

      for k := 0 to infUnidCarga.Count - 1 do
      begin
        sSecao := 'infUnidCarga'+ Nivel + IntToStrZero(J+1,3) + IntToStrZero(K+1, 3);

        with infUnidCarga.Items[k] do
        begin
          AINIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));
          AINIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
          AINIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);

          for l := 0 to lacUnidCarga.Count - 1 do
            AINIRec.WriteString('lacUnidCarga' + Nivel + IntToStrZero(J+1,3) + IntToStrZero(K+1, 3) + IntToStrZero(L+1, 3)
                               , 'nLacre'
                               , lacUnidCarga[l].nLacre);
        end;
      end;
    end;
  end;

  for j := 0 to infNFe[Idx2-1].infUnidCarga.Count - 1 do
  begin
    sSecao := 'infUnidCarga'+ Nivel + IntToStrZero(J+1,3);

    with infNFe[Idx2-1].infUnidCarga.Items[j] do
    begin
      AINIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(tpUnidCarga));
      AINIRec.WriteString(sSecao, 'idUnidCarga', idUnidCarga);
      AINIRec.WriteFloat(sSecao, 'qtdRat', qtdRat);

      for k := 0 to lacUnidCarga.Count - 1 do
        AINIRec.WriteString('lacUnidCarga' + Nivel + IntToStrZero(J+1,3) + IntToStrZero(k+1, 3)
                           , 'nLacre'
                           , lacUnidCarga[k].nLacre);
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_Total(AINIRec: TMemIniFile; total: Ttotal);
var
  sSecao: string;
begin
  sSecao := 'total';

  AINIRec.WriteFloat(sSecao, 'vTPrest', total.vTPrest);
  AINIRec.WriteFloat(sSecao, 'vTRec', total.vTRec);
end;

procedure TCTeIniWriter.Gerar_InfPercurso(AINIRec: TMemIniFile;
  infPercurso: TinfPercursoCollection);
var
  I: Integer;
begin
  for I := 0 to infPercurso.Count - 1 do
  begin
    AINIRec.WriteString('infPercurso'+IntToStrZero(I+1,3), 'UFPer', infPercurso[I].UFPer);
  end;
end;

procedure TCTeIniWriter.Gerar_InfServico(AINIRec: TMemIniFile;
  infServico: TInfServico);
begin
  AINIRec.WriteString('infServico', 'xDescServ', infServico.xDescServ);
  AINIRec.WriteString('infServico', 'qCarga', CurrToStr(infServico.qCarga));
end;

procedure TCTeIniWriter.Gerar_InfDocReferencia(AINIRec: TMemIniFile;
  infDocRef: TinfDocRefCollection);
var
  I: Integer;
  sSecao: string;
begin
  for i := 0 to infDocRef.Count -1 do
  begin
    sSecao := 'infDocRef' + IntToStrZero(I+1, 3);

    with infDocRef.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'chBPe', chBPe);
      AINIRec.WriteString(sSecao, 'nDoc', nDoc);
      AINIRec.WriteString(sSecao, 'serie', serie);
      AINIRec.WriteString(sSecao, 'serie', subserie);
      AINIRec.WriteString(sSecao, 'dEmi', DateToStr(dEmi));
      AINIRec.WriteString(sSecao, 'vDoc', CurrToStr(vDoc));
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_Seguro(AINIRec: TMemIniFile; seg: TSegCollection);
var
  I: Integer;
  sSecao: string;
begin
  for i:= 0 to seg.Count - 1 do
  begin
    sSecao := 'infSeg' + IntToStrZero(I+1, 3);

    with seg.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'respSeg', TpRspSeguroToStr(respSeg));
      AINIRec.WriteString(sSecao, 'xSeg', xSeg);
      AINIRec.WriteString(sSecao, 'nApol', nApol);
      AINIRec.WriteString(sSecao, 'nAver', nAver);
      AINIRec.WriteString(sSecao, 'vCarga', CurrToStr(vCarga));
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfModalRodoviarioOS(AINIRec: TMemIniFile;
  rodoOS: TRodoOS);
var
  sSecao: string;
begin
  sSecao := 'RodoOS';
  if (rodoOS.TAF <> '') or (rodoOS.NroRegEstadual <> '') then
  begin
    AINIRec.WriteString(sSecao, 'TAF', rodoOS.TAF);
    AINIRec.WriteString(sSecao, 'NroRegEstadual', rodoOS.NroRegEstadual);

    if rodoOS.veic.placa <> '' then
    begin
      sSecao := 'veic'+IntToStrZero(1,3);
      with rodoOS.veic do
      begin
        AINIRec.WriteString(sSecao, 'placa', placa);
        AINIRec.WriteString(sSecao, 'RENAVAM', RENAVAM);
        AINIRec.WriteString(sSecao, 'UF', UF);

        sSecao := 'prop'+IntToStrZero(1,3);

        AINIRec.WriteString(sSecao, 'CNPJCPF', prop.CNPJCPF);
        AINIRec.WriteString(sSecao, 'TAF', prop.TAF);
        AINIRec.WriteString(sSecao, 'NroRegEstadual', prop.NroRegEstadual);
        AINIRec.WriteString(sSecao, 'xNome', prop.xNome);
        AINIRec.WriteString(sSecao, 'IE', prop.IE);
        AINIRec.WriteString(sSecao, 'UF', prop.UF);
        AINIRec.WriteString(sSecao, 'tpProp', TpPropToStr(prop.tpProp));
      end;
    end;

    sSecao := 'infFretamento';
    with rodoOS.infFretamento do
    begin
      AINIRec.WriteString(sSecao, 'tpFretamento', TpFretamentoToStr(tpFretamento));
      AINIRec.WriteString(sSecao, 'dhViagem', DateTimeToStr(dhViagem));
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_InfCTeSub(AINIRec: TMemIniFile;
  infCteSub: TInfCteSub);
begin
  AINIRec.WriteString('infCteSub', 'chCte', infCteSub.chCte);
end;

procedure TCTeIniWriter.Gerar_InfGTVe(AINIRec: TMemIniFile;
  infGTVe: TinfGTVeCollection);
var
  I, J: Integer;
  sSecao: string;
begin
  for i := 0 to infGTVe.Count -1 do
  begin
    sSecao := 'infGTVe' + IntToStrZero(I+1, 3);

    with infGTVe[i] do
    begin
      AINIRec.WriteString(sSecao, 'chCTe', chCTe);

      for J := 0 to Comp.Count -1 do
      begin
        sSecao := 'infGTVeComp' + IntToStrZero(I+1, 3) + IntToStrZero(J+1, 3);

        AINIRec.WriteString(sSecao, 'tpComp', tpCompToStr(Comp[j].tpComp));
        AINIRec.WriteString(sSecao, 'vComp', CurrToStr(Comp[j].vComp));
        AINIRec.WriteString(sSecao, 'xComp', Comp[j].xComp);
      end;
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_Origem(AINIRec: TMemIniFile; origem: TEnderEmit);
begin
  AINIRec.WriteString('origem', 'xLgr', origem.xLgr);
  AINIRec.WriteString('origem', 'nro', origem.nro);
  AINIRec.WriteString('origem', 'xCpl', origem.xCpl);
  AINIRec.WriteString('origem', 'xBairro', origem.xBairro);
  AINIRec.WriteInteger('origem', 'cMun', origem.cMun);
  AINIRec.WriteString('origem', 'xMun', origem.xMun);
  AINIRec.WriteInteger('origem', 'CEP', origem.CEP);
  AINIRec.WriteString('origem', 'UF', origem.UF);
  AINIRec.WriteString('origem', 'fone', origem.fone);
end;

procedure TCTeIniWriter.Gerar_Destino(AINIRec: TMemIniFile;
  destino: TEnderEmit);
begin
  AINIRec.WriteString('destino', 'xLgr', destino.xLgr);
  AINIRec.WriteString('destino', 'nro', destino.nro);
  AINIRec.WriteString('destino', 'xCpl', destino.xCpl);
  AINIRec.WriteString('destino', 'xBairro', destino.xBairro);
  AINIRec.WriteInteger('destino', 'cMun', destino.cMun);
  AINIRec.WriteString('destino', 'xMun', destino.xMun);
  AINIRec.WriteInteger('destino', 'CEP', destino.CEP);
  AINIRec.WriteString('destino', 'UF', destino.UF);
  AINIRec.WriteString('destino', 'fone', destino.fone);
end;

procedure TCTeIniWriter.Gerar_DetalhamentoGTV(AINIRec: TMemIniFile;
  detGTV: TdetGTV);
var
  I: Integer;
  sSecao: string;
begin
  AINIRec.WriteString('detGTV', 'qCarga', CurrToStr(detGTV.qCarga));

  for i := 0 to detGTV.infEspecie.Count -1 do
  begin
    sSecao := 'infEspecie' + IntToStrZero(I+1, 3);

    with detGTV.infEspecie.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'tpEspecie', TEspecieToStr(tpEspecie));
      AINIRec.WriteString(sSecao, 'vEspecie', CurrToStr(vEspecie));
      AINIRec.WriteString(sSecao, 'tpNumerario', tpNumerarioToStr(tpNumerario));
      AINIRec.WriteString(sSecao, 'xMoedaEstr', xMoedaEstr);
    end;
  end;

  for i := 0 to detGTV.infVeiculo.Count -1 do
  begin
    sSecao := 'infVeiculo' + IntToStrZero(I+1, 3);

    with detGTV.infVeiculo.Items[i] do
    begin
      AINIRec.WriteString(sSecao, 'placa', placa);
      AINIRec.WriteString(sSecao, 'UF', UF);
      AINIRec.WriteString(sSecao, 'RNTRC', RNTRC);
    end;
  end;
end;

procedure TCTeIniWriter.Gerar_ProcessamentoCTe(AINIRec: TMemIniFile;
  procCTe: TprocCTe);
begin
  if (procCTe.cStat <> 0) then
  begin
    AINIRec.WriteString('procCTe', 'tpAmb', TpAmbToStr(procCTe.tpAmb));
    AINIRec.WriteString('procCTe', 'verAplic', procCTe.verAplic);
    AINIRec.WriteString('procCTe', 'chCTe', procCTe.chCTe);
    AINIRec.WriteString('procCTe', 'dhRecbto', DateTimeToStr(procCTe.dhRecbto));
    AINIRec.WriteString('procCTe', 'nProt', procCTe.nProt);
    AINIRec.WriteString('procCTe', 'digVal', procCTe.digVal);
    AINIRec.WriteString('procCTe', 'cStat', IntToStr(procCTe.cStat));
    AINIRec.WriteString('procCTe', 'xMotivo', procCTe.xMotivo);
  end;
end;

// Reforma Tributária
procedure TCTeIniWriter.Gerar_IBSCBS(AINIRec: TMemIniFile; IBSCBS: TIBSCBS);
var
  sSecao: string;
begin
  if (IBSCBS.gIBSCBS.vBC > 0) then
  begin
    sSecao := 'IBSCBS';

    AINIRec.WriteString(sSecao, 'CST', CSTIBSCBSToStr(IBSCBS.CST));
    AINIRec.WriteString(sSecao, 'cClassTrib', cClassTribToStr(IBSCBS.cClassTrib));

    if IBSCBS.gIBSCBS.vBC > 0 then
      Gerar_IBSCBS_gIBSCBS(AINIRec, IBSCBS.gIBSCBS);
  end;
end;

procedure TCTeIniWriter.Gerar_IBSCBS_gIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBS';

  AINIRec.WriteFloat(sSecao, 'vBC', gIBSCBS.vBC);
  AINIRec.WriteFloat(sSecao, 'vIBS', gIBSCBS.vIBS);

  Gerar_IBSCBS_gIBSCBS_gIBSUF(AINIRec, gIBSCBS.gIBSUF);
  Gerar_IBSCBS_gIBSCBS_gIBSMun(AINIRec, gIBSCBS.gIBSMun);
  Gerar_IBSCBS_gIBSCBS_gCBS(AINIRec, gIBSCBS.gCBS);

  if gIBSCBS.gTribRegular.pAliqEfetRegIBSUF > 0 then
    Gerar_IBSCBS_gIBSCBS_gTribReg(AINIRec, gIBSCBS.gTribRegular);

  if gIBSCBS.gIBSCredPres.pCredPres > 0 then
    Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec, gIBSCBS.gIBSCredPres, 'gIBSCredPres');

  if gIBSCBS.gCBSCredPres.pCredPres > 0 then
    Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec, gIBSCBS.gCBSCredPres, 'gCBSCredPres');

  if gIBSCBS.gTribCompraGov.pAliqIBSUF > 0 then
    Gerar_IBSCBS_gIBSCBS_gTribCompraGov(AINIRec, gIBSCBS.gTribCompraGov);
end;

procedure TCTeIniWriter.Gerar_IBSCBS_gIBSCBS_gIBSUF(AINIRec: TMemIniFile;
  gIBSUF: TgIBSUFValores);
var
  sSecao: string;
begin
  sSecao := 'gIBSUF';

  AINIRec.WriteFloat(sSecao, 'pIBSUF', gIBSUF.pIBS);
  AINIRec.WriteFloat(sSecao, 'vIBSUF', gIBSUF.vIBS);

  AINIRec.WriteFloat(sSecao, 'pDif', gIBSUF.gDif.pDif);
  AINIRec.WriteFloat(sSecao, 'vDif', gIBSUF.gDif.vDif);

  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSUF.gDevTrib.vDevTrib);

  AINIRec.WriteFloat(sSecao, 'pRedAliq', gIBSUF.gRed.pRedAliq);
  AINIRec.WriteFloat(sSecao, 'pAliqEfet', gIBSUF.gRed.pAliqEfet);
end;

procedure TCTeIniWriter.Gerar_IBSCBS_gIBSCBS_gIBSMun(AINIRec: TMemIniFile;
  gIBSMun: TgIBSMunValores);
var
  sSecao: string;
begin
  sSecao := 'gIBSMun';

  AINIRec.WriteFloat(sSecao, 'pIBSMun', gIBSMun.pIBS);
  AINIRec.WriteFloat(sSecao, 'vIBSMun', gIBSMun.vIBS);

  AINIRec.WriteFloat(sSecao, 'pDif', gIBSMun.gDif.pDif);
  AINIRec.WriteFloat(sSecao, 'vDif', gIBSMun.gDif.vDif);

  AINIRec.WriteFloat(sSecao, 'vDevTrib', gIBSMun.gDevTrib.vDevTrib);

  AINIRec.WriteFloat(sSecao, 'pRedAliq', gIBSMun.gRed.pRedAliq);
  AINIRec.WriteFloat(sSecao, 'pAliqEfet', gIBSMun.gRed.pAliqEfet);
end;

procedure TCTeIniWriter.Gerar_IBSCBS_gIBSCBS_gCBS(AINIRec: TMemIniFile;
  gCBS: TgCBSValores);
var
  sSecao: string;
begin
  sSecao := 'gCBS';

  AINIRec.WriteFloat(sSecao, 'pCBS', gCBS.pCBS);
  AINIRec.WriteFloat(sSecao, 'vCBS', gCBS.vCBS);

  AINIRec.WriteFloat(sSecao, 'pDif', gCBS.gDif.pDif);
  AINIRec.WriteFloat(sSecao, 'vDif', gCBS.gDif.vDif);

  AINIRec.WriteFloat(sSecao, 'vDevTrib', gCBS.gDevTrib.vDevTrib);

  AINIRec.WriteFloat(sSecao, 'pRedAliq', gCBS.gRed.pRedAliq);
  AINIRec.WriteFloat(sSecao, 'pAliqEfet', gCBS.gRed.pAliqEfet);
end;

procedure TCTeIniWriter.Gerar_IBSCBS_gIBSCBS_gTribReg(AINIRec: TMemIniFile;
  gTribRegular: TgTribRegular);
var
  sSecao: string;
begin
  sSecao := 'gTribRegular';

  AINIRec.WriteString(sSecao, 'CSTReg', CSTIBSCBSToStr(gTribRegular.CSTReg));
  AINIRec.WriteString(sSecao, 'cClassTribReg', cClassTribToStr(gTribRegular.cClassTribReg));
  AINIRec.WriteFloat(sSecao, 'pAliqEfetRegIBSUF', gTribRegular.pAliqEfetRegIBSUF);
  AINIRec.WriteFloat(sSecao, 'vTribRegIBSUF', gTribRegular.vTribRegIBSUF);
  AINIRec.WriteFloat(sSecao, 'pAliqEfetRegIBSMun', gTribRegular.pAliqEfetRegIBSMun);
  AINIRec.WriteFloat(sSecao, 'vTribRegIBSMun', gTribRegular.vTribRegIBSMun);
  AINIRec.WriteFloat(sSecao, 'pAliqEfetRegCBS', gTribRegular.pAliqEfetRegCBS);
  AINIRec.WriteFloat(sSecao, 'vTribRegCBS', gTribRegular.vTribRegCBS);
end;

procedure TCTeIniWriter.Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(AINIRec: TMemIniFile;
  gIBSCredPres: TgIBSCBSCredPres; const Grupo: string);
var
  sSecao: string;
begin
  sSecao := Grupo;

  AINIRec.WriteString(sSecao, 'cCredPres', cCredPresToStr(gIBSCredPres.cCredPres));
  AINIRec.WriteFloat(sSecao, 'pCredPres', gIBSCredPres.pCredPres);
  AINIRec.WriteFloat(sSecao, 'vCredPres', gIBSCredPres.vCredPres);
  AINIRec.WriteFloat(sSecao, 'vCredPresCondSus', gIBSCredPres.vCredPresCondSus);
end;

procedure TCTeIniWriter.Gerar_IBSCBS_gIBSCBS_gTribCompraGov(
  AINIRec: TMemIniFile; gTribCompraGov: TgTribCompraGov);
var
  sSecao: string;
begin
  sSecao := 'gTribCompraGov';

  AINIRec.WriteFloat(sSecao, 'pAliqIBSUF', gTribCompraGov.pAliqIBSUF);
  AINIRec.WriteFloat(sSecao, 'vTribIBSUF', gTribCompraGov.vTribIBSUF);
  AINIRec.WriteFloat(sSecao, 'pAliqIBSMun', gTribCompraGov.pAliqIBSMun);
  AINIRec.WriteFloat(sSecao, 'vTribIBSMun', gTribCompraGov.vTribIBSMun);
  AINIRec.WriteFloat(sSecao, 'pAliqCBS', gTribCompraGov.pAliqCBS);
  AINIRec.WriteFloat(sSecao, 'vTribCBS', gTribCompraGov.vTribCBS);
end;

end.
