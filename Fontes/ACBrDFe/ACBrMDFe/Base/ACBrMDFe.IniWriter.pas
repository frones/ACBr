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

unit ACBrMDFe.IniWriter;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrMDFe.Classes,
//  ACBrMDFe.Conversao,
  ACBrDFeComum.Proc,
//  pmdfeProcMDFe,
  pmdfeConversaoMDFe;

type
  { TMDFeIniWriter }

  TMDFeIniWriter = class
  private
    FMDFe: TMDFe;

    procedure Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Gerar_Carregamento(AINIRec: TMemIniFile; infMunCarrega: TinfMunCarregaCollection);
    procedure Gerar_Percurso(AINIRec: TMemIniFile; infPercurso: TinfPercursoCollection);
    procedure Gerar_Emitente(AINIRec: TMemIniFile; Emit: Temit);

    procedure Gerar_ModalRodoviario(AINIRec: TMemIniFile; rodo: Trodo);
    procedure Gerar_InfANTT(AINIRec: TMemIniFile; infANTT: TinfANTT);
    procedure Gerar_InfCIOT(AINIRec: TMemIniFile; infCIOT: TinfCIOTCollection);
    procedure Gerar_ValePedagio(AINIRec: TMemIniFile; valePed: TvalePed);
    procedure Gerar_Dispositivo(AINIRec: TMemIniFile; disp: TdispCollection);
    procedure Gerar_InfContratante(AINIRec: TMemIniFile; infContratante: TinfContratanteCollection);
    procedure Gerar_InfPagamento(AINIRec: TMemIniFile; infPag: TinfPagCollection);
    procedure Gerar_Componentes(AINIRec: TMemIniFile; Comp: TCompCollection; Idx: Integer);
    procedure Gerar_InfPrazos(AINIRec: TMemIniFile; infPrazo: TInfPrazoCollection; Idx: Integer);
    procedure Gerar_InfBanco(AINIRec: TMemIniFile; infBanc: TinfBanc; Idx: Integer);
    procedure Gerar_VeiculoTracao(AINIRec: TMemIniFile; veicTracao: TveicTracao);
    procedure Gerar_Motoristas(AINIRec: TMemIniFile; condutor: TcondutorCollection);
    procedure Gerar_VeiculoReboque(AINIRec: TMemIniFile; veicReboque: TveicReboqueCollection);
    procedure Gerar_LacresRodo(AINIRec: TMemIniFile; lacRodo: TlacRodoCollection);

    procedure Gerar_ModalAereo(AINIRec: TMemIniFile; aereo: Taereo);

    procedure Gerar_ModalAquaviario(AINIRec: TMemIniFile; aquav: Taquav);
    procedure Gerar_InfTerminalCarregamento(AINIRec: TMemIniFile; infTermCarreg: TinfTermCarregCollection);
    procedure Gerar_InfTerminalDescarregamento(AINIRec: TMemIniFile; infTermDescarreg: TinfTermDescarregCollection);
    procedure Gerar_InfEmbComboio(AINIRec: TMemIniFile; infEmbComb: TinfEmbCombCollection);
    procedure Gerar_InfUnidCargaVazia(AINIRec: TMemIniFile; infUnidCargaVazia: TinfUnidCargaVaziaCollection);
    procedure Gerar_InfUnidTranspVazia(AINIRec: TMemIniFile; infUnidTranspVazia: TinfUnidTranspVaziaCollection);

    procedure Gerar_ModalFerroviario(AINIRec: TMemIniFile; ferrov: Tferrov);
    procedure Gerar_Vagoes(AINIRec: TMemIniFile; vag: TvagCollection);

    procedure Gerar_Descarregamento(AINIRec: TMemIniFile; infMunDescarga: TinfMunDescargaCollection);
    procedure Gerar_InfCTe(AINIRec: TMemIniFile; infCTe: TinfCTeCollection; Idx: Integer);
    procedure Gerar_Peri(AINIRec: TMemIniFile; peri: TPeriCTeCollection; Idx1, Idx2: Integer);
    procedure Gerar_InfNFePrestParcial(AINIRec: TMemIniFile; infNFePrestParcial: TinfNFePrestParcialCollection; Idx1, Idx2: Integer);
    procedure Gerar_InfEntregaParcial(AINIRec: TMemIniFile; infEntregaParcial: TinfEntregaParcial; Idx1, Idx2: Integer);
    procedure Gerar_InfUnidTransp(AINIRec: TMemIniFile; infUnidTransp: TinfUnidTranspCTeCollection; Idx1, Idx2: Integer);
    procedure Gerar_LacUnidTransp(AINIRec: TMemIniFile; lacUnidTransp: TlacresCollection; Idx1, Idx2, Idx3: Integer);
    procedure Gerar_InfUnidCarga(AINIRec: TMemIniFile; infUnidCarga: TinfUnidCargaCollection; Idx1, Idx2, Idx3: Integer);
    procedure Gerar_LacUnidCarga(AINIRec: TMemIniFile; lacUnidCarga: TlacUnidCargaCollection; Idx1, Idx2, Idx3, Idx4: Integer);

    procedure Gerar_InfNFe(AINIRec: TMemIniFile; infNFe: TinfNFeCollection; Idx: Integer);
    procedure Gerar_InfMDFeTransp(AINIRec: TMemIniFile; infMDFeTransp: TinfMDFeTranspCollection; Idx: Integer);

    procedure Gerar_Seguro(AINIRec: TMemIniFile; seg: TSegCollection);
    procedure Gerar_Averbacao(AINIRec: TMemIniFile; aver: TAverCollection; Idx: Integer);

    procedure Gerar_ProdutoPredominante(AINIRec: TMemIniFile; prodPred: TprodPred);
    procedure Gerar_Totais(AINIRec: TMemIniFile; tot: Ttot);
    procedure Gerar_Lacres(AINIRec: TMemIniFile; lacres: TlacresCollection);
    procedure Gerar_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Gerar_InfAdicionais(AINIRec: TMemIniFile; infAdic: TinfAdic);
    procedure Gerar_InfRespTecnico(AINIRec: TMemIniFile; infRespTec: TinfRespTec);
    procedure Gerar_ProcMDFe(AINIRec: TMemIniFile; procMDFe: TProcDFe);
  public
    constructor Create(AOwner: TMDFe); reintroduce;

    function GravarIni: string;

    property MDFe: TMDFe read FMDFe write FMDFe;
  end;


implementation

uses
  ACBrXmlBase,
  ACBrDFeUtil,
  ACBrMDFe,
  ACBrUtil.Base,
  pcnConversao;

{ TMDFeIniWriter }

constructor TMDFeIniWriter.Create(AOwner: TMDFe);
begin
  inherited Create;

  FMDFe := AOwner;
end;

function TMDFeIniWriter.GravarIni: string;
var
  INIRec: TMemIniFile;
  IniMDFe: TStringList;
begin
  Result := '';

  if not ValidarChave(FMDFe.infMDFe.ID) then
    raise EACBrMDFeException.Create('MDFe Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    INIRec.WriteString('infMDFe', 'ID', FMDFe.infMDFe.ID);
    INIRec.WriteString('infMDFe', 'versao', FloatToStr(FMDFe.infMDFe.Versao));

    Gerar_Identificacao(INIRec, FMDFe.Ide);
    Gerar_Carregamento(INIRec, FMDFe.Ide.infMunCarrega);
    Gerar_Percurso(INIRec, FMDFe.Ide.infPercurso);
    Gerar_Emitente(INIRec, FMDFe.Emit);

    case FMDFe.Ide.modal of
      moRodoviario: Gerar_ModalRodoviario(INIRec, FMDFe.rodo);
      moAereo: Gerar_ModalAereo(INIRec, FMDFe.aereo);
      moAquaviario: Gerar_ModalAquaviario(INIRec, FMDFe.aquav);
      moFerroviario: Gerar_ModalFerroviario(INIRec, FMDFe.ferrov);
    end;

    Gerar_Descarregamento(INIRec, FMDFe.infDoc.infMunDescarga);
    Gerar_Seguro(INIRec, FMDFe.seg);
    Gerar_ProdutoPredominante(INIRec, FMDFe.prodPred);
    Gerar_Totais(INIRec, FMDFe.tot);
    Gerar_Lacres(INIRec, FMDFe.lacres);
    Gerar_AutorizadosXml(INIRec, FMDFe.autXML);
    Gerar_InfAdicionais(INIRec, FMDFe.infAdic);
    Gerar_InfRespTecnico(INIRec, FMDFe.infRespTec);
    Gerar_ProcMDFe(INIRec, FMDFe.procMDFe);

    IniMDFe := TStringList.Create;
    try
      INIRec.GetStrings(IniMDFe);
      Result := StringReplace(IniMDFe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniMDFe.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

procedure TMDFeIniWriter.Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  sSecao: string;
begin
  sSecao := 'ide';

  AINIRec.WriteInteger(sSecao, 'cUF', Ide.cUF);
  AINIRec.WriteString(sSecao, 'tpEmit', TpEmitenteToStr(Ide.tpEmit));
  AINIRec.WriteString(sSecao, 'mod', Ide.modelo);
  AINIRec.WriteInteger(sSecao, 'serie', Ide.serie);
  AINIRec.WriteInteger(sSecao, 'nMDF', Ide.nMDF);
  AINIRec.WriteInteger(sSecao, 'cMDF', Ide.cMDF);
  AINIRec.WriteString(sSecao, 'modal', ModalToStr(Ide.modal));
  AINIRec.WriteString(sSecao, 'dhEmi', DateToStr(Ide.dhEmi));
  AINIRec.WriteString(sSecao, 'tpEmis', TpEmisToStr(Ide.tpEmis));
  AINIRec.WriteString(sSecao, 'procEmi', procEmiToStr(Ide.procEmi));
  AINIRec.WriteString(sSecao, 'verProc', Ide.verProc);
  AINIRec.WriteString(sSecao, 'UFIni', Ide.UFIni);
  AINIRec.WriteString(sSecao, 'UFFim', Ide.UFFim);
  AINIRec.WriteString(sSecao, 'dhIniViagem', DateToStr(Ide.dhIniViagem));
  AINIRec.WriteString(sSecao, 'tpTransp', TTransportadorToStr(Ide.tpTransp));
  AINIRec.WriteString(sSecao, 'indCanalVerde', TindicadorToStr(Ide.indCanalVerde));
  AINIRec.WriteString(sSecao, 'indCarregaPosterior', TindicadorToStr(Ide.indCarregaPosterior));
end;

procedure TMDFeIniWriter.Gerar_Carregamento(AINIRec: TMemIniFile;
  infMunCarrega: TinfMunCarregaCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infMunCarrega.Count - 1 do
  begin
    sSecao := 'CARR' + IntToStrZero(I + 1, 3);

    AINIRec.WriteInteger(sSecao, 'cMunCarrega', infMunCarrega[i].cMunCarrega);
    AINIRec.WriteString(sSecao, 'xMunCarrega', infMunCarrega[i].xMunCarrega);
  end;
end;

procedure TMDFeIniWriter.Gerar_Percurso(AINIRec: TMemIniFile;
  infPercurso: TinfPercursoCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infPercurso.Count - 1 do
  begin
    sSecao := 'PERC' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'UFPer', infPercurso[i].UFPer);
  end;
end;

procedure TMDFeIniWriter.Gerar_Emitente(AINIRec: TMemIniFile; Emit: Temit);
var
  sSecao: string;
begin
  sSecao := 'emit';

  AINIRec.WriteString(sSecao, 'CNPJ', Emit.CNPJCPF);
  AINIRec.WriteString(sSecao, 'IE', Emit.IE);
  AINIRec.WriteString(sSecao, 'xNome', Emit.xNome);
  AINIRec.WriteString(sSecao, 'xFant', Emit.xFant);
  AINIRec.WriteString(sSecao, 'xLgr', Emit.enderEmit.xLgr);
  AINIRec.WriteString(sSecao, 'nro', Emit.enderEmit.nro);
  AINIRec.WriteString(sSecao, 'xCpl', Emit.enderEmit.xCpl);
  AINIRec.WriteString(sSecao, 'xBairro', Emit.enderEmit.xBairro);
  AINIRec.WriteInteger(sSecao, 'cMun', Emit.enderEmit.cMun);
  AINIRec.WriteString(sSecao, 'xMun', Emit.enderEmit.xMun);
  AINIRec.WriteInteger(sSecao, 'CEP', Emit.enderEmit.CEP);
  AINIRec.WriteString(sSecao, 'UF', Emit.enderEmit.UF);
  AINIRec.WriteString(sSecao, 'fone', Emit.enderEmit.fone);
  AINIRec.WriteString(sSecao, 'email', Emit.enderEmit.email);
end;

procedure TMDFeIniWriter.Gerar_ModalRodoviario(AINIRec: TMemIniFile;
  rodo: Trodo);
var
  sSecao: string;
begin
  sSecao := 'Rodo';

  AINIRec.WriteString(sSecao, 'codAgPorto', Rodo.codAgPorto);

  Gerar_InfANTT(AINIRec, rodo.infANTT);
  Gerar_InfCIOT(AINIRec, rodo.infANTT.infCIOT);
  Gerar_ValePedagio(AINIRec, rodo.infANTT.valePed);
  Gerar_Dispositivo(AINIRec, rodo.infANTT.valePed.disp);
  Gerar_InfContratante(AINIRec, rodo.infANTT.infContratante);
  Gerar_InfPagamento(AINIRec, rodo.infANTT.infPag);

  Gerar_VeiculoTracao(AINIRec, rodo.veicTracao);
  Gerar_VeiculoReboque(AINIRec, rodo.veicReboque);
  Gerar_LacresRodo(AINIRec, rodo.lacRodo);
end;

procedure TMDFeIniWriter.Gerar_InfANTT(AINIRec: TMemIniFile; infANTT: TinfANTT);
var
  sSecao: string;
begin
  sSecao := 'infANTT';

  AINIRec.WriteString(sSecao, 'RNTRC', infANTT.RNTRC);
end;

procedure TMDFeIniWriter.Gerar_InfCIOT(AINIRec: TMemIniFile;
  infCIOT: TinfCIOTCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infCIOT.Count - 1 do
  begin
    sSecao := 'infCIOT' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'CIOT', infCIOT[i].CIOT);
    AINIRec.WriteString(sSecao, 'CNPJCPF', infCIOT[i].CNPJCPF);
  end;
end;

procedure TMDFeIniWriter.Gerar_ValePedagio(AINIRec: TMemIniFile;
  valePed: TvalePed);
var
  sSecao: string;
begin
  sSecao := 'valePed';

  AINIRec.WriteString(sSecao, 'categCombVeic', categCombVeicToStr(valePed.categCombVeic));
end;

procedure TMDFeIniWriter.Gerar_Dispositivo(AINIRec: TMemIniFile;
  disp: TdispCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to disp.Count - 1 do
  begin
    sSecao := 'disp' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'CNPJForn', disp[i].CNPJForn);
    AINIRec.WriteString(sSecao, 'CNPJPg', disp[i].CNPJPg);
    AINIRec.WriteString(sSecao, 'nCompra', disp[i].nCompra);
    AINIRec.WriteFloat(sSecao, 'vValePed', disp[i].vValePed);
    AINIRec.WriteString(sSecao, 'tpValePed', tpValePedToStr(disp[i].tpValePed));
  end;
end;

procedure TMDFeIniWriter.Gerar_InfContratante(AINIRec: TMemIniFile;
  infContratante: TinfContratanteCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infContratante.Count - 1 do
  begin
    sSecao := 'infContratante' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'CNPJCPF', infContratante[i].CNPJCPF);
    AINIRec.WriteString(sSecao, 'idEstrangeiro', infContratante[i].idEstrangeiro);
    AINIRec.WriteString(sSecao, 'xNome', infContratante[i].xNome);

    if (infContratante[i].infContrato.NroContrato <> '') and
       (infContratante[i].infContrato.vContratoGlobal <> 0) then
    begin
      AINIRec.WriteString(sSecao, 'NroContrato', infContratante[i].infContrato.NroContrato);
      AINIRec.WriteFloat(sSecao, 'vContratoGlobal', infContratante[i].infContrato.vContratoGlobal);
    end;
  end;
end;

procedure TMDFeIniWriter.Gerar_InfPagamento(AINIRec: TMemIniFile;
  infPag: TinfPagCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infPag.Count - 1 do
  begin
    sSecao := 'infPag' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'xNome', infPag[i].xNome);
    AINIRec.WriteString(sSecao, 'idEstrangeiro', infPag[i].idEstrangeiro);
    AINIRec.WriteString(sSecao, 'CNPJCPF', infPag[i].CNPJCPF);
    AINIRec.WriteFloat(sSecao, 'vContrato', infPag[i].vContrato);
    AINIRec.WriteString(sSecao, 'indAltoDesemp', indAltoDesempToStr(infPag[i].indAltoDesemp));
    AINIRec.WriteString(sSecao, 'indPag', TindPagToStr(infPag[i].indPag));
    AINIRec.WriteFloat(sSecao, 'vAdiant', infPag[i].vAdiant);
    AINIRec.WriteString(sSecao, 'indAntecipaAdiant', TIndicadorToStr(infPag[i].indAntecipaAdiant));
    AINIRec.WriteString(sSecao, 'tpAntecip', tpAntecipToStr(infPag[i].tpAntecip));

    Gerar_Componentes(AINIRec, infPag[i].Comp, i);

    if infPag[i].indPag = ipPrazo then
    begin
      Gerar_InfPrazos(AINIRec, infPag[i].infPrazo, i);
    end;

    Gerar_InfBanco(AINIRec, infPag[i].infBanc, i);
  end;
end;

procedure TMDFeIniWriter.Gerar_Componentes(AINIRec: TMemIniFile;
  Comp: TCompCollection; Idx: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to Comp.Count - 1 do
  begin
    sSecao := 'Comp' + IntToStrZero(Idx + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'tpComp', TCompToStr(Comp[i].tpComp));
    AINIRec.WriteFloat(sSecao, 'vComp', Comp[i].vComp);
    AINIRec.WriteString(sSecao, 'xComp', Comp[i].xComp);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfPrazos(AINIRec: TMemIniFile;
  infPrazo: TInfPrazoCollection; Idx: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infPrazo.Count - 1 do
  begin
    sSecao := 'infPrazo' + IntToStrZero(Idx + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteInteger(sSecao, 'nParcela', infPrazo[i].nParcela);
    AINIRec.WriteString(sSecao, 'dVenc', DateToStr(infPrazo[i].dVenc));
    AINIRec.WriteFloat(sSecao, 'vParcela', infPrazo[i].vParcela);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfBanco(AINIRec: TMemIniFile; infBanc: TinfBanc;
  Idx: Integer);
var
  sSecao: string;
begin
  sSecao := 'infBanc' + IntToStrZero(Idx, 3);

  AINIRec.WriteString(sSecao, 'PIX', infBanc.PIX);
  AINIRec.WriteString(sSecao, 'CNPJIPEF', infBanc.CNPJIPEF);
  AINIRec.WriteString(sSecao, 'codBanco', infBanc.codBanco);
  AINIRec.WriteString(sSecao, 'codAgencia', infBanc.codAgencia);
end;

procedure TMDFeIniWriter.Gerar_VeiculoTracao(AINIRec: TMemIniFile;
  veicTracao: TveicTracao);
var
  sSecao: string;
begin
  sSecao := 'veicTracao';

  if (veicTracao.placa <> '') then
  begin
    AINIRec.WriteString(sSecao, 'clInt', veicTracao.cInt);
    AINIRec.WriteString(sSecao, 'placa', veicTracao.placa);
    AINIRec.WriteString(sSecao, 'RENAVAN', veicTracao.RENAVAM);
    AINIRec.WriteInteger(sSecao, 'tara', veicTracao.tara);
    AINIRec.WriteInteger(sSecao, 'capKG', veicTracao.capKG);
    AINIRec.WriteInteger(sSecao, 'capM3', veicTracao.capM3);
    AINIRec.WriteString(sSecao, 'tpRod', TpRodadoToStr(veicTracao.tpRod));
    AINIRec.WriteString(sSecao, 'tpCar', TpCarroceriaToStr(veicTracao.tpCar));
    AINIRec.WriteString(sSecao, 'UF', veicTracao.UF);

    AINIRec.WriteString(sSecao, 'CNPJCPF', veicTracao.prop.CNPJCPF);
    AINIRec.WriteString(sSecao, 'RNTRC', veicTracao.prop.RNTRC);
    AINIRec.WriteString(sSecao, 'xNome', veicTracao.prop.xNome);
    AINIRec.WriteString(sSecao, 'IE', veicTracao.prop.IE);
    AINIRec.WriteString(sSecao, 'UFProp', veicTracao.prop.UF);
    AINIRec.WriteString(sSecao, 'tpProp', TpPropToStr(veicTracao.prop.tpProp));

    Gerar_Motoristas(AINIRec, veicTracao.condutor);
  end;
end;

procedure TMDFeIniWriter.Gerar_Motoristas(AINIRec: TMemIniFile;
  condutor: TcondutorCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to condutor.Count - 1 do
  begin
    sSecao := 'moto' + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'CPF', condutor[i].CPF);
    AINIRec.WriteString(sSecao, 'xNome', condutor[i].xNome);
  end;
end;

procedure TMDFeIniWriter.Gerar_VeiculoReboque(AINIRec: TMemIniFile;
  veicReboque: TveicReboqueCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to veicReboque.Count - 1 do
  begin
    sSecao := 'reboque' + IntToStrZero(i + 1, 2);

    AINIRec.WriteString(sSecao, 'clInt', veicReboque[i].cInt);
    AINIRec.WriteString(sSecao, 'placa', veicReboque[i].placa);
    AINIRec.WriteString(sSecao, 'RENAVAN', veicReboque[i].RENAVAM);
    AINIRec.WriteInteger(sSecao, 'tara', veicReboque[i].tara);
    AINIRec.WriteInteger(sSecao, 'capKG', veicReboque[i].capKG);
    AINIRec.WriteInteger(sSecao, 'capM3', veicReboque[i].capM3);
    AINIRec.WriteString(sSecao, 'tpCar', TpCarroceriaToStr(veicReboque[i].tpCar));
    AINIRec.WriteString(sSecao, 'UF', veicReboque[i].UF);

    AINIRec.WriteString(sSecao, 'CNPJCPF', veicReboque[i].prop.CNPJCPF);
    AINIRec.WriteString(sSecao, 'RNTRC', veicReboque[i].prop.RNTRC);
    AINIRec.WriteString(sSecao, 'xNome', veicReboque[i].prop.xNome);
    AINIRec.WriteString(sSecao, 'IE', veicReboque[i].prop.IE);
    AINIRec.WriteString(sSecao, 'UFProp', veicReboque[i].prop.UF);
    AINIRec.WriteString(sSecao, 'tpProp', TpPropToStr(veicReboque[i].prop.tpProp));
  end;
end;

procedure TMDFeIniWriter.Gerar_LacresRodo(AINIRec: TMemIniFile;
  lacRodo: TlacRodoCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to lacRodo.Count - 1 do
  begin
    sSecao := 'lacRodo' + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'nLacre', lacRodo[i].nLacre);
  end;
end;

procedure TMDFeIniWriter.Gerar_ModalAereo(AINIRec: TMemIniFile; aereo: Taereo);
var
  sSecao: string;
begin
  sSecao := 'aereo';
  AINIRec.WriteString(sSecao, 'matr', Aereo.matr);
  AINIRec.WriteString(sSecao, 'nVoo', Aereo.nVoo);
  AINIRec.WriteString(sSecao, 'cAerEmb', Aereo.cAerEmb);
  AINIRec.WriteString(sSecao, 'cAerDes', Aereo.cAerDes);
  AINIRec.WriteString(sSecao, 'dVoo', DateToStr(Aereo.dVoo));
end;

procedure TMDFeIniWriter.Gerar_ModalAquaviario(AINIRec: TMemIniFile;
  aquav: Taquav);
var
  sSecao: string;
begin
  sSecao := 'aquav';

  AINIRec.WriteString(sSecao, 'CNPJAgeNav', Aquav.CNPJAgeNav);
  AINIRec.WriteString(sSecao, 'irin', Aquav.irin);
  AINIRec.WriteString(sSecao, 'tpEmb', Aquav.tpEmb);
  AINIRec.WriteString(sSecao, 'cEmbar', Aquav.cEmbar);
  AINIRec.WriteString(sSecao, 'xEmbar', Aquav.xEmbar);
  AINIRec.WriteString(sSecao, 'cPrtEmb', Aquav.cPrtEmb);
  AINIRec.WriteString(sSecao, 'cPrtDest', Aquav.cPrtDest);
  AINIRec.WriteString(sSecao, 'prtTrans', Aquav.prtTrans);
  AINIRec.WriteString(sSecao, 'tpNav', TpNavegacaoToStr(Aquav.tpNav));
  AINIRec.WriteString(sSecao, 'MMSI', Aquav.MMSI);

  Gerar_InfTerminalCarregamento(AINIRec, aquav.infTermCarreg);
  Gerar_InfTerminalDescarregamento(AINIRec, aquav.infTermDescarreg);
  Gerar_InfEmbComboio(AINIRec, aquav.infEmbComb);
  Gerar_InfUnidCargaVazia(AINIRec, aquav.infUnidCargaVazia);
  Gerar_InfUnidTranspVazia(AINIRec, aquav.infUnidTranspVazia);
end;

procedure TMDFeIniWriter.Gerar_InfTerminalCarregamento(AINIRec: TMemIniFile;
  infTermCarreg: TinfTermCarregCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infTermCarreg.Count - 1 do
  begin
    sSecao := 'infTermCarreg' + IntToStrZero(i + 1, 1);

    AINIRec.WriteString(sSecao, 'cTermCarreg', infTermCarreg[i].cTermCarreg);
    AINIRec.WriteString(sSecao, 'xTermCarreg', infTermCarreg[i].xTermCarreg);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfTerminalDescarregamento(AINIRec: TMemIniFile;
  infTermDescarreg: TinfTermDescarregCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infTermDescarreg.Count - 1 do
  begin
    sSecao := 'infTermDescarreg' + IntToStrZero(i + 1, 1);

    AINIRec.WriteString(sSecao, 'cTermDescarreg', infTermDescarreg[i].cTermDescarreg);
    AINIRec.WriteString(sSecao, 'xTermDescarreg', infTermDescarreg[i].xTermDescarreg);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfEmbComboio(AINIRec: TMemIniFile;
  infEmbComb: TinfEmbCombCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infEmbComb.Count - 1 do
  begin
    sSecao := 'infEmbComb' + IntToStrZero(i + 1, 2);

    AINIRec.WriteString(sSecao, 'cEmbComb', infEmbComb[i].cEmbComb);
    AINIRec.WriteString(sSecao, 'xBalsa', infEmbComb[i].xBalsa);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfUnidCargaVazia(AINIRec: TMemIniFile;
  infUnidCargaVazia: TinfUnidCargaVaziaCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infUnidCargaVazia.Count - 1 do
  begin
    sSecao := 'infUnidCargaVazia' + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'idUnidCargaVazia', infUnidCargaVazia[i].idUnidCargaVazia);
    AINIRec.WriteString(sSecao, 'tpUnidCargaVazia', UnidCargaToStr(infUnidCargaVazia[i].tpUnidCargaVazia));
  end;
end;

procedure TMDFeIniWriter.Gerar_InfUnidTranspVazia(AINIRec: TMemIniFile;
  infUnidTranspVazia: TinfUnidTranspVaziaCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infUnidTranspVazia.Count - 1 do
  begin
    sSecao := 'infUnidTranspVazia' + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'idUnidTranspVazia', infUnidTranspVazia[i].idUnidTranspVazia);
    AINIRec.WriteString(sSecao, 'tpUnidTranspVazia', UnidTranspToStr(infUnidTranspVazia[i].tpUnidTranspVazia));
  end;
end;

procedure TMDFeIniWriter.Gerar_ModalFerroviario(AINIRec: TMemIniFile;
  ferrov: Tferrov);
var
  sSecao: string;
begin
  sSecao := 'ferrov';

  AINIRec.WriteString(sSecao, 'xPref', Ferrov.xPref);
  AINIRec.WriteString(sSecao, 'dhTrem', DateToStr(Ferrov.dhTrem));
  AINIRec.WriteString(sSecao, 'xOri', Ferrov.xOri);
  AINIRec.WriteString(sSecao, 'xDest', Ferrov.xDest);
  AINIRec.WriteInteger(sSecao, 'qVag', Ferrov.qVag);

  Gerar_Vagoes(AINIRec, Ferrov.vag);
end;

procedure TMDFeIniWriter.Gerar_Vagoes(AINIRec: TMemIniFile;
  vag: TvagCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to vag.Count - 1 do
  begin
    sSecao := 'vag' + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'serie', vag[i].serie);
    AINIRec.WriteInteger(sSecao, 'nVag', vag[i].nVag);
    AINIRec.WriteInteger(sSecao, 'nSeq', vag[i].nSeq);
    AINIRec.WriteFloat(sSecao, 'TU', vag[i].TU);
    AINIRec.WriteFloat(sSecao, 'pesoBC', vag[i].pesoBC);
    AINIRec.WriteFloat(sSecao, 'pesoR', vag[i].pesoR);
    AINIRec.WriteString(sSecao, 'tpVag', vag[i].tpVag);
  end;
end;

procedure TMDFeIniWriter.Gerar_Descarregamento(AINIRec: TMemIniFile;
  infMunDescarga: TinfMunDescargaCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infMunDescarga.Count - 1 do
  begin
    sSecao := 'DESC' + IntToStrZero(I + 1, 3);

    AINIRec.WriteInteger(sSecao, 'cMunDescarga', infMunDescarga[i].cMunDescarga);
    AINIRec.WriteString(sSecao, 'xMunDescarga', infMunDescarga[i].xMunDescarga);

    Gerar_InfCTe(AINIRec, infMunDescarga[i].infCTe, I);
    Gerar_InfNFe(AINIRec, infMunDescarga[i].infNFe, I);
    Gerar_InfMDFeTransp(AINIRec, infMunDescarga[i].infMDFeTransp, I);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfCTe(AINIRec: TMemIniFile;
  infCTe: TinfCTeCollection; Idx: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infCTe.Count - 1 do
  begin
    sSecao := 'infCTe' + IntToStrZero(Idx + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'chCTe', infCTe[i].chCTe);
    AINIRec.WriteString(sSecao, 'SegCodBarra', infCTe[i].SegCodBarra);
    AINIRec.WriteString(sSecao, 'indReentrega', infCTe[i].indReentrega);
    AINIRec.WriteString(sSecao, 'indPrestacaoParcial', TIndicadorExToStr(infCTe[i].indPrestacaoParcial));

    Gerar_Peri(AINIRec, infCTe[i].peri, Idx, i);
    Gerar_InfNFePrestParcial(AINIRec, infCTe[i].infNFePrestParcial, Idx, i);
    Gerar_InfEntregaParcial(AINIRec, infCTe[i].infEntregaParcial, Idx, i);
    Gerar_InfUnidTransp(AINIRec, infCTe[i].infUnidTransp, Idx, i);
  end;
end;

procedure TMDFeIniWriter.Gerar_Peri(AINIRec: TMemIniFile;
  peri: TPeriCTeCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to peri.Count - 1 do
  begin
    sSecao := 'peri' + IntToStrZero(Idx1 + 1, 3) +
                    IntToStrZero(Idx2 + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'nONU', peri[i].nONU);
    AINIRec.WriteString(sSecao, 'xNomeAE', peri[i].xNomeAE);
    AINIRec.WriteString(sSecao, 'xClaRisco', peri[i].xClaRisco);
    AINIRec.WriteString(sSecao, 'grEmb', peri[i].grEmb);
    AINIRec.WriteString(sSecao, 'qTotProd', peri[i].qTotProd);
    AINIRec.WriteString(sSecao, 'qVolTipo', peri[i].qVolTipo);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfNFePrestParcial(AINIRec: TMemIniFile;
  infNFePrestParcial: TinfNFePrestParcialCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infNFePrestParcial.Count - 1 do
  begin
    sSecao := 'infNFePrestParcial' + IntToStrZero(Idx1 + 1, 3) +
                    IntToStrZero(Idx2 + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'chNFe', infNFePrestParcial[i].chNFe);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfEntregaParcial(AINIRec: TMemIniFile;
  infEntregaParcial: TinfEntregaParcial; Idx1, Idx2: Integer);
var
  sSecao: string;
begin
  sSecao := 'infEntregaParcial' + IntToStrZero(Idx1 + 1, 3) + IntToStrZero(Idx2 + 1, 3);

  AINIRec.WriteFloat(sSecao, 'qtdTotal', infEntregaParcial.qtdTotal);
  AINIRec.WriteFloat(sSecao, 'qtdParcial', infEntregaParcial.qtdParcial);
end;

procedure TMDFeIniWriter.Gerar_InfUnidTransp(AINIRec: TMemIniFile;
  infUnidTransp: TinfUnidTranspCTeCollection; Idx1, Idx2: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infUnidTransp.Count - 1 do
  begin
    sSecao := 'infUnidTransp' + IntToStrZero(Idx1 + 1, 3) +
                    IntToStrZero(Idx2 + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'idUnidTransp', infUnidTransp[i].idUnidTransp);
    AINIRec.WriteFloat(sSecao, 'qtdRat', infUnidTransp[i].qtdRat);
    AINIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(infUnidTransp[i].tpUnidTransp));

    Gerar_LacUnidTransp(AINIRec, infUnidTransp[i].lacUnidTransp, Idx1, Idx2, i);
    Gerar_InfUnidCarga(AINIRec, infUnidTransp[i].infUnidCarga, Idx1, Idx2, i);
  end;
end;

procedure TMDFeIniWriter.Gerar_LacUnidTransp(AINIRec: TMemIniFile;
  lacUnidTransp: TlacresCollection; Idx1, Idx2, Idx3: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to lacUnidTransp.Count - 1 do
  begin
    sSecao := 'lacUnidTransp' + IntToStrZero(Idx1 + 1, 3) +
               IntToStrZero(Idx2 + 1, 3) + IntToStrZero(Idx3 + 1, 3) +
               IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'nLacre', lacUnidTransp[i].nLacre);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfUnidCarga(AINIRec: TMemIniFile;
  infUnidCarga: TinfUnidCargaCollection; Idx1, Idx2, Idx3: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infUnidCarga.Count - 1 do
  begin
    sSecao := 'infUnidCarga' + IntToStrZero(Idx1 + 1, 3) +
               IntToStrZero(Idx2 + 1, 3) + IntToStrZero(Idx3 + 1, 3) +
               IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'idUnidCarga', infUnidCarga[i].idUnidCarga);
    AINIRec.WriteFloat(sSecao, 'qtdRat', infUnidCarga[i].qtdRat);
    AINIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(infUnidCarga[i].tpUnidCarga));

    Gerar_LacUnidCarga(AINIRec, infUnidCarga[i].lacUnidCarga, Idx1, Idx2, Idx3, i);
  end;
end;

procedure TMDFeIniWriter.Gerar_LacUnidCarga(AINIRec: TMemIniFile;
  lacUnidCarga: TlacUnidCargaCollection; Idx1, Idx2, Idx3, Idx4: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to lacUnidCarga.Count - 1 do
  begin
    sSecao := 'lacUnidCarga' + IntToStrZero(Idx1 + 1, 3) +
               IntToStrZero(Idx2 + 1, 3) + IntToStrZero(Idx3 + 1, 3) +
               IntToStrZero(Idx4 + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'nLacre', lacUnidCarga[i].nLacre);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfNFe(AINIRec: TMemIniFile;
  infNFe: TinfNFeCollection; Idx: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infNFe.Count - 1 do
  begin
    sSecao := 'infNFe' + IntToStrZero(Idx + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'chNFe', infNFe[i].chNFe);
    AINIRec.WriteString(sSecao, 'SegCodBarra', infNFe[i].SegCodBarra);
    AINIRec.WriteString(sSecao, 'indReentrega', infNFe[i].indReentrega);

    Gerar_Peri(AINIRec, infNFe[i].peri, Idx, i);
    Gerar_InfUnidTransp(AINIRec, infNFe[i].infUnidTransp, Idx, i);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfMDFeTransp(AINIRec: TMemIniFile;
  infMDFeTransp: TinfMDFeTranspCollection; Idx: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to infMDFeTransp.Count - 1 do
  begin
    sSecao := 'infMDFeTransp' + IntToStrZero(Idx + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'chMDFe', infMDFeTransp[i].chMDFe);
    AINIRec.WriteString(sSecao, 'indReentrega', infMDFeTransp[i].indReentrega);

    Gerar_Peri(AINIRec, infMDFeTransp[i].peri, Idx, i);
    Gerar_InfUnidTransp(AINIRec, infMDFeTransp[i].infUnidTransp, Idx, i);
  end;
end;

procedure TMDFeIniWriter.Gerar_Seguro(AINIRec: TMemIniFile;
  seg: TSegCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to seg.Count - 1 do
  begin
    sSecao := 'seg' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'respSeg', RspSeguroMDFeToStr(seg[i].respSeg));
    AINIRec.WriteString(sSecao, 'CNPJCPF', seg[i].CNPJCPF);
    AINIRec.WriteString(sSecao, 'xSeg', seg[i].xSeg);
    AINIRec.WriteString(sSecao, 'CNPJ', seg[i].CNPJ);
    AINIRec.WriteString(sSecao, 'nApol', seg[i].nApol);

    Gerar_Averbacao(AINIRec, seg[i].aver, I);
  end;
end;

procedure TMDFeIniWriter.Gerar_Averbacao(AINIRec: TMemIniFile;
  aver: TAverCollection; Idx: Integer);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to aver.Count - 1 do
  begin
    sSecao := 'aver' + IntToStrZero(Idx + 1, 3) + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'nAver', aver[i].nAver);
  end;
end;

procedure TMDFeIniWriter.Gerar_ProdutoPredominante(AINIRec: TMemIniFile;
  prodPred: TprodPred);
var
  sSecao: string;
begin
  sSecao := 'prodPred';

  AINIRec.WriteString(sSecao, 'tpCarga', TCargaToStr(prodPred.tpCarga));
  AINIRec.WriteString(sSecao, 'xProd', prodPred.xProd);
  AINIRec.WriteString(sSecao, 'cEAN', prodPred.cEAN);
  AINIRec.WriteString(sSecao, 'NCM', prodPred.NCM);

  sSecao := 'infLocalCarrega';

  AINIRec.WriteInteger(sSecao, 'CEP', prodPred.infLocalCarrega.CEP);
  AINIRec.WriteFloat(sSecao, 'latitude', prodPred.infLocalCarrega.latitude);
  AINIRec.WriteFloat(sSecao, 'longitude', prodPred.infLocalCarrega.longitude);

  sSecao := 'infLocalDescarrega';

  AINIRec.WriteInteger(sSecao, 'CEP', prodPred.infLocalDescarrega.CEP);
  AINIRec.WriteFloat(sSecao, 'latitude', prodPred.infLocalDescarrega.latitude);
  AINIRec.WriteFloat(sSecao, 'longitude', prodPred.infLocalDescarrega.longitude);
end;

procedure TMDFeIniWriter.Gerar_Totais(AINIRec: TMemIniFile; tot: Ttot);
var
  sSecao: string;
begin
  sSecao := 'tot';

  AINIRec.WriteInteger(sSecao, 'qCTe', tot.qCTe);
  AINIRec.WriteInteger(sSecao, 'qNFe', tot.qNFe);
  AINIRec.WriteInteger(sSecao, 'qMDFe', tot.qMDFe);
  AINIRec.WriteFloat(sSecao, 'vCarga', tot.vCarga);
  AINIRec.WriteFloat(sSecao, 'qCarga', tot.qCarga);
  AINIRec.WriteString(sSecao, 'cUnid', UnidMedToStr(tot.cUnid));
end;

procedure TMDFeIniWriter.Gerar_Lacres(AINIRec: TMemIniFile;
  lacres: TlacresCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to lacres.Count - 1 do
  begin
    sSecao := 'lacres' + IntToStrZero(I + 1, 3);

    AINIRec.WriteString(sSecao, 'nLacre', lacres[i].nLacre);
  end;
end;

procedure TMDFeIniWriter.Gerar_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to autXML.Count - 1 do
  begin
    sSecao := 'autXML' + IntToStrZero(I + 1, 2);

    AINIRec.WriteString(sSecao, 'CNPJCPF', autXML[i].CNPJCPF);
  end;
end;

procedure TMDFeIniWriter.Gerar_InfAdicionais(AINIRec: TMemIniFile;
  infAdic: TinfAdic);
var
  sSecao: string;
begin
  sSecao := 'infAdic';

  AINIRec.WriteString(sSecao, 'infAdFisco', infAdic.infAdFisco);
  AINIRec.WriteString(sSecao, 'infCpl', infAdic.infCpl);
end;

procedure TMDFeIniWriter.Gerar_InfRespTecnico(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
var
  sSecao: string;
begin
  sSecao := 'infRespTec';

  AINIRec.WriteString(sSecao, 'CNPJ', infRespTec.CNPJ);
  AINIRec.WriteString(sSecao, 'xContato', infRespTec.xContato);
  AINIRec.WriteString(sSecao, 'email', infRespTec.email);
  AINIRec.WriteString(sSecao, 'fone', infRespTec.fone);
end;

procedure TMDFeIniWriter.Gerar_ProcMDFe(AINIRec: TMemIniFile;
  procMDFe: TProcDFe);
var
  sSecao: string;
begin
  sSecao := 'procMDFe';

  AINIRec.WriteString(sSecao, 'tpAmb', TipoAmbienteToStr(procMDFe.tpAmb));
  AINIRec.WriteString(sSecao, 'verAplic', procMDFe.verAplic);
  AINIRec.WriteString(sSecao, 'chMDFe', procMDFe.chDFe);
  AINIRec.WriteString(sSecao, 'dhRecbto', DateTimeToStr(procMDFe.dhRecbto));
  AINIRec.WriteString(sSecao, 'nProt', procMDFe.nProt);
  AINIRec.WriteString(sSecao, 'digVal', procMDFe.digVal);
  AINIRec.WriteString(sSecao, 'cStat', IntToStr(procMDFe.cStat));
  AINIRec.WriteString(sSecao, 'xMotivo', procMDFe.xMotivo);
end;

end.
