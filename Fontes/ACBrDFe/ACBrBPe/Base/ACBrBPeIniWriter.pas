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

unit ACBrBPeIniWriter;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrBPeClass,
  ACBrBPeConversao;

type
  { TBPeIniWriter }

  TBPeIniWriter = class
  private
    FBPe: TBPe;
    {
    FVersaoDF: TVersaoBPe;
    FAmbiente: Integer;
    FtpEmis: Integer;

    }
    procedure Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Gerar_Comprador(AINIRec: TMemIniFile; Comp: TComp);
    procedure Gerar_Agencia(AINIRec: TMemIniFile; Agencia: TAgencia);
    procedure Gerar_InfBPeSub(AINIRec: TMemIniFile; infBPeSub: TInfBPeSub);
    procedure Gerar_InfPassagem(AINIRec: TMemIniFile; infPassagem: TInfPassagem);
    procedure Gerar_InfPassageiro(AINIRec: TMemIniFile; infPassageiro: TInfPassageiro);
    procedure Gerar_InfViagem(AINIRec: TMemIniFile; infViagem: TInfViagemCollection);
    procedure Gerar_InfValorBPe(AINIRec: TMemIniFile; infValorBPe: TinfValorBPe);
    procedure Gerar_Componentes(AINIRec: TMemIniFile; Comp: TCompCollection);
    procedure Gerar_Impostos(AINIRec: TMemIniFile; Imp: TImp);
    procedure Gerar_ICMS(AINIRec: TMemIniFile; ICMS: TICMS);
    procedure Gerar_ICMSUFFim(AINIRec: TMemIniFile; ICMSUFFim: TICMSUFFim);
    procedure Gerar_Pagamentos(AINIRec: TMemIniFile; Pag: TpagCollection);
    procedure Gerar_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Gerar_InfAdic(AINIRec: TMemIniFile; InfAdic: TInfAdic);
    procedure Gerar_InfRespTec(AINIRec: TMemIniFile; infRespTec: TinfRespTec);
  public
    constructor Create(AOwner: TBPe); reintroduce;

    function GravarIni: string;

    property BPe: TBPe read FBPe write FBPe;
    {
    property VersaoDF: TVersaoBPe read FVersaoDF write FVersaoDF;
    property Ambiente: Integer read FAmbiente write FAmbiente;
    property tpEmis: Integer read FtpEmis write FtpEmis;
    }
  end;


implementation

uses
  ACBrXmlBase,
  ACBrDFeUtil,
  ACBrBPe,
  ACBrUtil.Base;

{ TBPeIniWriter }

constructor TBPeIniWriter.Create(AOwner: TBPe);
begin
  inherited Create;

  FBPe := AOwner;
end;

function TBPeIniWriter.GravarIni: string;
var
  INIRec: TMemIniFile;
  IniBPe: TStringList;
  Ok: Boolean;
begin
  Result := '';

  if not ValidarChave(FBPe.infBPe.ID) then
    raise EACBrBPeException.Create('BPe Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    INIRec.WriteString('infBPe', 'versao', VersaoBPeToStr(DblToVersaoBPe(Ok, FBPe.infBPe.versao)));

    Gerar_Identificacao(INIRec, FBPe.Ide);
    Gerar_Emitente(INIRec, FBPe.Emit);
    Gerar_Comprador(INIRec, FBPe.comp);
    Gerar_Agencia(INIRec, FBPe.agencia);
    Gerar_InfBPeSub(INIRec, FBPe.infBPeSub);
    Gerar_InfPassagem(INIRec, FBPe.infPassagem);
    Gerar_InfPassageiro(INIRec, FBPe.infPassagem.infPassageiro);
    Gerar_InfViagem(INIRec, FBPe.infViagem);
    Gerar_InfValorBPe(INIRec, FBPe.infValorBPe);
    Gerar_Componentes(INIRec, FBPe.infValorBPe.Comp);
    Gerar_Impostos(INIRec, FBPe.Imp);
    Gerar_Pagamentos(INIRec, FBPe.pag);
    Gerar_AutorizadosXml(INIRec, FBPe.autXML);
    Gerar_InfAdic(INIRec, FBPe.InfAdic);
    Gerar_InfRespTec(INIRec, FBPe.infRespTec);

    IniBPe := TStringList.Create;
    try
      INIRec.GetStrings(IniBPe);
      Result := StringReplace(IniBPe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniBPe.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

procedure TBPeIniWriter.Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
begin
  // Identificação

  AINIRec.WriteInteger('ide', 'cUF', ide.cUF);
  AINIRec.WriteInteger('ide', 'mod', Ide.modelo);
  AINIRec.WriteInteger('ide', 'serie', Ide.serie);
  AINIRec.WriteInteger('ide', 'nBP', Ide.nBP);
  AINIRec.WriteInteger('ide', 'cBP', Ide.cBP);
  AINIRec.WriteString('ide', 'modal', ModalBPeToStr(Ide.modal));
  AINIRec.WriteString('ide', 'dhEmi', DateTimeToStr(Ide.dhEmi));
  AINIRec.WriteString('ide', 'tpEmis', TipoEmissaoToStr(Ide.tpEmis));
  AINIRec.WriteString('ide', 'verProc', Ide.verProc);
  AINIRec.WriteString('ide', 'tpBPe', tpBPeToStr(Ide.tpBPe));
  AINIRec.WriteString('ide','indPres', PresencaCompradorToStr(Ide.indPres));
  AINIRec.WriteString('ide', 'UFIni', Ide.UFIni);
  AINIRec.WriteInteger('ide', 'cMunIni', Ide.cMunIni);
  AINIRec.WriteString('ide', 'UFFim', Ide.UFFim);
  AINIRec.WriteInteger('ide', 'cMunFim', Ide.cMunFim);

  if Ide.dhCont > 0 then
    AINIRec.WriteString('ide', 'dhCont', DateTimeToStr(Ide.dhCont))
  else
    AINIRec.WriteString('ide', 'dhCont', '');

  AINIRec.WriteString('ide', 'xJust', Ide.xJust);
end;

procedure TBPeIniWriter.Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
begin
  // Emitente

  AINIRec.WriteString('emit', 'CNPJ', Emit.CNPJ);
  AINIRec.WriteString('emit', 'IE', Emit.IE);
  AINIRec.WriteString('emit', 'IEST', Emit.IEST);
  AINIRec.WriteString('emit', 'xNome', Emit.xNome);
  AINIRec.WriteString('emit', 'xFant', Emit.xFant);
  AINIRec.WriteString('emit', 'TAR', Emit.TAR);
  AINIRec.WriteString('emit', 'CRT', CRTToStr(Emit.CRT));

  AINIRec.WriteString('emit', 'xLgr', Emit.enderEmit.xLgr);
  AINIRec.WriteString('emit', 'nro', Emit.enderEmit.nro);
  AINIRec.WriteString('emit', 'xCpl', Emit.enderEmit.xCpl);
  AINIRec.WriteString('emit', 'xBairro', Emit.enderEmit.xBairro);
  AINIRec.WriteInteger('emit', 'cMun', Emit.enderEmit.cMun);
  AINIRec.WriteString('emit', 'xMun', Emit.enderEmit.xMun);
  AINIRec.WriteInteger('emit', 'CEP', Emit.enderEmit.CEP);
  AINIRec.WriteString('emit', 'UF', Emit.enderEmit.UF);
  AINIRec.WriteString('emit', 'fone', Emit.enderEmit.fone);
  AINIRec.WriteString('emit', 'email', Emit.enderEmit.Email);
end;

procedure TBPeIniWriter.Gerar_Comprador(AINIRec: TMemIniFile; Comp: TComp);
begin
  // Comprador

  AINIRec.WriteString('comp', 'xNome', comp.xNome);
  AINIRec.WriteString('comp', 'CNPJCPF', comp.CNPJCPF);
  AINIRec.WriteString('comp', 'idEstrangeiro', comp.idEstrangeiro);
  AINIRec.WriteString('comp', 'IE', comp.IE);

  AINIRec.WriteString('comp', 'xLgr', comp.EnderComp.xLgr);
  AINIRec.WriteString('comp', 'nro', comp.EnderComp.nro);
  AINIRec.WriteString('comp', 'xCpl', comp.EnderComp.xCpl);
  AINIRec.WriteString('comp', 'xBairro', comp.EnderComp.xBairro);
  AINIRec.WriteInteger('comp', 'cMun', comp.EnderComp.cMun);
  AINIRec.WriteString('comp', 'xMun', comp.EnderComp.xMun);
  AINIRec.WriteInteger('comp', 'CEP', comp.EnderComp.CEP);
  AINIRec.WriteString('comp', 'UF', comp.EnderComp.UF);
  AINIRec.WriteInteger('comp', 'cPais', comp.EnderComp.cPais);
  AINIRec.WriteString('comp', 'xPais', comp.EnderComp.xPais);
  AINIRec.WriteString('comp', 'fone', comp.EnderComp.fone);
  AINIRec.WriteString('comp', 'email', Comp.EnderComp.email);
end;

procedure TBPeIniWriter.Gerar_Agencia(AINIRec: TMemIniFile; Agencia: TAgencia);
begin
  // Agencia

  AINIRec.WriteString('agencia', 'xNome', agencia.xNome);
  AINIRec.WriteString('agencia', 'CNPJ', agencia.CNPJ);

  AINIRec.WriteString('agencia', 'xLgr', agencia.EnderAgencia.xLgr);
  AINIRec.WriteString('agencia', 'nro', agencia.EnderAgencia.nro);
  AINIRec.WriteString('agencia', 'xCpl', agencia.EnderAgencia.xCpl);
  AINIRec.WriteString('agencia', 'xBairro', agencia.EnderAgencia.xBairro);
  AINIRec.WriteInteger('agencia', 'cMun', agencia.EnderAgencia.cMun);
  AINIRec.WriteString('agencia', 'xMun', agencia.EnderAgencia.xMun);
  AINIRec.WriteInteger('agencia', 'CEP', agencia.EnderAgencia.CEP);
  AINIRec.WriteString('agencia', 'UF', agencia.EnderAgencia.UF);
  AINIRec.WriteInteger('agencia', 'cPais', agencia.EnderAgencia.cPais);
  AINIRec.WriteString('agencia', 'xPais', agencia.EnderAgencia.xPais);
  AINIRec.WriteString('agencia', 'fone', agencia.EnderAgencia.fone);
  AINIRec.WriteString('agencia', 'email', agencia.EnderAgencia.email);
end;

procedure TBPeIniWriter.Gerar_InfBPeSub(AINIRec: TMemIniFile;
  infBPeSub: TInfBPeSub);
begin
  // Informações dos BP-e de Substituição

  AINIRec.WriteString('infBPeSub', 'chBPe', infBPeSub.chBPe);
  AINIRec.WriteString('infBPeSub', 'tpSub', tpSubstituicaoToStr(infBpeSub.tpSub));
end;

procedure TBPeIniWriter.Gerar_InfPassagem(AINIRec: TMemIniFile;
  infPassagem: TInfPassagem);
begin
  // Informações da Passagem

  AINIRec.WriteString('infPassagem', 'cLocOrig', infPassagem.cLocOrig);
  AINIRec.WriteString('infPassagem', 'xLocOrig', infPassagem.xLocOrig);
  AINIRec.WriteString('infPassagem', 'cLocDest', infPassagem.cLocDest);
  AINIRec.WriteString('infPassagem', 'xLocDest', infPassagem.xLocDest);
  AINIRec.WriteString('infPassagem', 'dhEmb', DateTimeToStr(infPassagem.dhEmb));
  AINIRec.WriteString('infPassagem', 'dhValidade', DateTimeToStr(infPassagem.dhValidade));
end;

procedure TBPeIniWriter.Gerar_InfPassageiro(AINIRec: TMemIniFile;
  infPassageiro: TInfPassageiro);
begin
  // Informações do Passageiro

  if infPassageiro.xNome <> '' then
  begin
    AINIRec.WriteString('infPassageiro', 'xNome', infPassageiro.xNome);
    AINIRec.WriteString('infPassageiro', 'CPF', infPassageiro.CPF);
    AINIRec.WriteString('infPassageiro', 'tpDoc', tpDocumentoToStr(infPassageiro.tpDoc));
    AINIRec.WriteString('infPassageiro', 'nDoc', infPassageiro.nDoc);
    AINIRec.WriteString('infPassageiro', 'xDoc', infPassageiro.xDoc);
    AINIRec.WriteString('infPassageiro', 'dNasc', DateToStr(infPassageiro.dNasc));
    AINIRec.WriteString('infPassageiro', 'fone', infPassageiro.Fone);
    AINIRec.WriteString('infPassageiro', 'email', infPassageiro.Email);
  end;
end;

procedure TBPeIniWriter.Gerar_InfViagem(AINIRec: TMemIniFile;
  infViagem: TInfViagemCollection);
var
  i: integer;
  sSecao: string;
begin
   // Informações da Viagem

  for i := 0 to infViagem.Count - 1 do
  begin
    sSecao := 'infViagem' + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'cPercurso', infViagem[i].cPercurso);
    AINIRec.WriteString(sSecao, 'xPercurso', infViagem[i].xPercurso);
    AINIRec.WriteString(sSecao, 'tpViagem', tpViagemToStr(infViagem[i].tpViagem));
    AINIRec.WriteString(sSecao, 'tpServ', tpServicoToStr(infViagem[i].tpServ));
    AINIRec.WriteString(sSecao, 'tpAcomodacao', tpAcomodacaoToStr(infViagem[i].tpAcomodacao));
    AINIRec.WriteString(sSecao, 'tpTrecho', tpTrechoToStr(infViagem[i].tpTrecho));
    AINIRec.WriteString(sSecao, 'dhViagem', DateTimeToStr(infViagem[i].dhViagem));

    if infViagem[i].dhConexao > 0 then
      AINIRec.WriteString(sSecao, 'dhConexao', DateTimeToStr(infViagem[i].dhConexao))
    else
    AINIRec.WriteString(sSecao, 'dhConexao', '');

    AINIRec.WriteString(sSecao, 'prefixo', infViagem[i].Prefixo);
    AINIRec.WriteInteger(sSecao, 'poltrona', infViagem[i].Poltrona);
    AINIRec.WriteString(sSecao, 'plataforma', infViagem[i].Plataforma);

    // Informações da Travessia

    if infViagem[i].infTravessia.tpVeiculo <> tvNenhum then
    begin
      AINIRec.WriteString(sSecao, 'tpVeiculo', tpVeiculoToStr(infViagem[i].infTravessia.tpVeiculo));
      AINIRec.WriteString(sSecao, 'sitVeiculo', SitVeiculoToStr(infViagem[i].infTravessia.sitVeiculo));
    end;
  end;
end;

procedure TBPeIniWriter.Gerar_InfValorBPe(AINIRec: TMemIniFile;
  infValorBPe: TinfValorBPe);
begin
  // Informações sobre os Valores do BPe

  AINIRec.WriteFloat('infValorBPe', 'vBP', infValorBPe.vBP);
  AINIRec.WriteFloat('infValorBPe', 'vDesconto', infValorBPe.vDesconto);
  AINIRec.WriteFloat('infValorBPe', 'vPgto', infValorBPe.vPgto);
  AINIRec.WriteFloat('infValorBPe', 'vTroco', infValorBPe.vTroco);
  AINIRec.WriteString('infValorBPe', 'tpDesconto', tpDescontoToStr(infValorBPe.tpDesconto));
  AINIRec.WriteString('infValorBPe', 'xDesconto', infValorBPe.xDesconto);
  AINIRec.WriteString('infValorBPe', 'cDesconto', infValorBPe.cDesconto);
end;

procedure TBPeIniWriter.Gerar_Componentes(AINIRec: TMemIniFile;
  Comp: TCompCollection);
var
  i: integer;
  sSecao: string;
begin
  // Componentes do Valor

  for i := 0 to Comp.Count - 1 do
  begin
    sSecao := 'Comp' + IntToStrZero(i + 1, 3);

    AINIRec.WriteString(sSecao, 'tpComp', tpComponenteToStr(Comp[i].tpComp));
    AINIRec.WriteFloat(sSecao, 'vComp', Comp[i].vComp);
  end;
end;

procedure TBPeIniWriter.Gerar_Impostos(AINIRec: TMemIniFile; Imp: TImp);
begin
  // Impostos

  Gerar_ICMS(AINIRec, BPe.Imp.ICMS);

  AINIRec.WriteFloat('ICMS', 'vTotTrib', Imp.vTotTrib);
  AINIRec.WriteString('ICMS', 'infAdFisco', Imp.infAdFisco);

  Gerar_ICMSUFFim(AINIRec, BPe.Imp.ICMSUFFim);
end;

procedure TBPeIniWriter.Gerar_ICMS(AINIRec: TMemIniFile; ICMS: TICMS);
begin
  // ICMS

  AINIRec.WriteString('ICMS', 'CST', CSTICMSTOStr(ICMS.CST));
  AINIRec.WriteFloat('ICMS', 'vBC', ICMS.vBC);
  AINIRec.WriteFloat('ICMS', 'pICMS', ICMS.pICMS);
  AINIRec.WriteFloat('ICMS', 'vICMS', ICMS.vICMS);
  AINIRec.WriteFloat('ICMS', 'pRedBC', ICMS.pRedBC);
  AINIRec.WriteFloat('ICMS', 'vCred', ICMS.vCred);
  AINIRec.WriteFloat('ICMS', 'vICMSDeson', ICMS.vICMSDeson);
  AINIRec.WriteString('ICMS', 'cBenef', ICMS.cBenef);
end;

procedure TBPeIniWriter.Gerar_ICMSUFFim(AINIRec: TMemIniFile;
  ICMSUFFim: TICMSUFFim);
begin
  // ICMSUFFim

  AINIRec.WriteFloat('ICMSUFFim', 'vBCUFFim', ICMSUFFim.vBCUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'pFCPUFFim', ICMSUFFim.pFCPUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'pICMSUFFim', ICMSUFFim.pICMSUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'pICMSInter', ICMSUFFim.pICMSInter);
  AINIRec.WriteFloat('ICMSUFFim', 'pICMSInterPart', ICMSUFFim.pICMSInterPart);
  AINIRec.WriteFloat('ICMSUFFim', 'vFCPUFFim', ICMSUFFim.vFCPUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'vICMSUFFim', ICMSUFFim.vICMSUFFim);
  AINIRec.WriteFloat('ICMSUFFim', 'vICMSUFIni', ICMSUFFim.vICMSUFIni);
end;

procedure TBPeIniWriter.Gerar_Pagamentos(AINIRec: TMemIniFile;
  Pag: TpagCollection);
var
  i: integer;
  sSecao: string;
begin
  // Pagamentos

  for i := 0 to pag.Count - 1 do
  begin
    sSecao := 'pag' + IntToStrZero(i + 1, 2);

    AINIRec.WriteString(sSecao, 'tPag', FormaPagamentoBPeToStr(pag[I].tPag));
    AINIRec.WriteString(sSecao, 'xPag', pag[I].xPag);
    AINIRec.WriteString(sSecao, 'nDocPag', pag[I].nDocPag);
    AINIRec.WriteFloat(sSecao, 'vPag', pag[I].vPag);
    AINIRec.WriteString(sSecao, 'tpIntegra', tpIntegraToStr(pag[I].tpIntegra));
    AINIRec.WriteString(sSecao, 'CNPJ', pag[I].CNPJ);
    AINIRec.WriteString(sSecao, 'tBand', BandeiraCardToStr(pag[I].tBand));
    AINIRec.WriteString(sSecao, 'xBand', pag[I].xBand);
    AINIRec.WriteString(sSecao, 'cAut', pag[I].cAut);
    AINIRec.WriteString(sSecao, 'nsuTrans', pag[I].nsuTrans);
    AINIRec.WriteString(sSecao, 'nsuHost', pag[I].nsuHost);
    AINIRec.WriteInteger(sSecao, 'nParcelas', pag[I].nParcelas);
    AINIRec.WriteString(sSecao, 'infAdCard', pag[I].infAdCard);
  end;
end;

procedure TBPeIniWriter.Gerar_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  i: integer;
  sSecao: string;
begin
  // Autorizados a baixar o XML

  for i := 0 to autXML.Count - 1 do
  begin
    sSecao := 'autXML' + IntToStrZero(i + 1, 2);

    AINIRec.WriteString(sSecao, 'CNPJCPF', autXML.Items[i].CNPJCPF);
  end;
end;

procedure TBPeIniWriter.Gerar_InfAdic(AINIRec: TMemIniFile; InfAdic: TInfAdic);
begin
  // Informações Adicionais

  AINIRec.WriteString('infAdic', 'infAdFisco', InfAdic.infAdFisco);
  AINIRec.WriteString('infAdic', 'infCpl', InfAdic.infCpl);
end;

procedure TBPeIniWriter.Gerar_InfRespTec(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
begin
  // Informações do Responsável Técnico

  AINIRec.WriteString('infRespTec', 'CNPJ', infRespTec.CNPJ);
  AINIRec.WriteString('infRespTec', 'xContato', infRespTec.xContato);
  AINIRec.WriteString('infRespTec', 'email', infRespTec.email);
  AINIRec.WriteString('infRespTec', 'fone', infRespTec.fone);
end;

end.
