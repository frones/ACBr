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

unit ACBrBPeIniReader;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrBPeClass,
  ACBrBPeConversao;

type
  { TBPeIniReader }

  TBPeIniReader = class
  private
    FBPe: TBPe;
    FVersaoDF: TVersaoBPe;
    FAmbiente: Integer;
    FtpEmis: Integer;

    procedure Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Ler_Comprador(AINIRec: TMemIniFile; Comp: TComp);
    procedure Ler_Agencia(AINIRec: TMemIniFile; Agencia: TAgencia);
    procedure Ler_InfBPeSub(AINIRec: TMemIniFile; infBPeSub: TInfBPeSub);
    procedure Ler_InfPassagem(AINIRec: TMemIniFile; infPassagem: TInfPassagem);
    procedure Ler_InfPassageiro(AINIRec: TMemIniFile; infPassageiro: TInfPassageiro);
    procedure Ler_InfViagem(AINIRec: TMemIniFile; infViagem: TInfViagemCollection);
    procedure Ler_InfValorBPe(AINIRec: TMemIniFile; infValorBPe: TinfValorBPe);
    procedure Ler_Componentes(AINIRec: TMemIniFile; Comp: TCompCollection);
    procedure Ler_Impostos(AINIRec: TMemIniFile; Imp: TImp);
    procedure Ler_ICMS(AINIRec: TMemIniFile; ICMS: TICMS);
    procedure Ler_ICMSUFFim(AINIRec: TMemIniFile; ICMSUFFim: TICMSUFFim);
    procedure Ler_Pagamentos(AINIRec: TMemIniFile; Pag: TpagCollection);
    procedure Ler_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Ler_InfAdic(AINIRec: TMemIniFile; InfAdic: TInfAdic);
    procedure Ler_InfRespTec(AINIRec: TMemIniFile; infRespTec: TinfRespTec);
  public
    constructor Create(AOwner: TBPe); reintroduce;

    function LerIni(const AIniString: string): Boolean;

    property BPe: TBPe read FBPe write FBPe;
    property VersaoDF: TVersaoBPe read FVersaoDF write FVersaoDF;
    property Ambiente: Integer read FAmbiente write FAmbiente;
    property tpEmis: Integer read FtpEmis write FtpEmis;
  end;

implementation

uses
  ACBrXmlBase,
  ACBrBPe,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

{ TBPeIniReader }

constructor TBPeIniReader.Create(AOwner: TBPe);
begin
  inherited Create;

  FBPe := AOwner;
end;

function TBPeIniReader.LerIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    FBPe.infBPe.versao := StringToFloatDef(INIRec.ReadString('infBPe', 'versao', VersaoBPeToStr(VersaoDF)),0);

    Ler_Identificacao(INIRec, FBPe.Ide);
    Ler_Emitente(INIRec, FBPe.Emit);

    FBPe.ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigoUF(FBPe.Emit.enderEmit.UF));

    Ler_Comprador(INIRec, FBPe.Comp);
    Ler_Agencia(INIRec, FBPe.Agencia);
    Ler_InfBPeSub(INIRec, FBPe.infBPeSub);
    Ler_InfPassagem(INIRec, FBPe.infPassagem);
    Ler_InfPassageiro(INIRec, FBPe.infPassagem.infPassageiro);
    Ler_InfViagem(INIRec, FBPe.infViagem);
    Ler_InfValorBPe(INIRec, FBPe.infValorBPe);
    Ler_Componentes(INIRec, FBPe.infValorBPe.Comp);
    Ler_Impostos(INIRec, FBPe.Imp);
    Ler_Pagamentos(INIRec, FBPe.Pag);
    Ler_AutorizadosXml(INIRec, FBPe.autXML);
    Ler_InfAdic(INIRec, FBPe.InfAdic);
    Ler_InfRespTec(INIRec, FBPe.infRespTec);
  finally
    INIRec.Free;
  end;
end;

procedure TBPeIniReader.Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  OK: Boolean;
begin
  //
  // Seção [Ide] Identificação do BP-e
  //
  Ide.tpAmb   := StrToTipoAmbiente(OK, AINIRec.ReadString('ide', 'tpAmb', IntToStr(Ambiente)));
  Ide.modelo  := AINIRec.ReadInteger('ide', 'mod', 63);
  Ide.serie   := AINIRec.ReadInteger('ide', 'serie', 1);
  Ide.nBP     := AINIRec.ReadInteger('ide', 'nBP', 0);
  Ide.cBP     := AINIRec.ReadInteger('ide', 'cBP', 0);
  Ide.modal   := StrToModalBPe(OK, AINIRec.ReadString('ide', 'modal', '1'));
  Ide.dhEmi   := StringToDateTime(AINIRec.ReadString('ide', 'dhEmi', '0'));
  Ide.tpEmis  := StrToTipoEmissao(OK, AINIRec.ReadString('ide', 'tpEmis', IntToStr(tpEmis)));
  Ide.verProc := AINIRec.ReadString('ide', 'verProc', 'ACBrBPe');
  Ide.tpBPe   := StrTotpBPe(OK, AINIRec.ReadString('ide', 'tpBPe', '0'));
  Ide.indPres := StrToPresencaComprador(OK, AINIRec.ReadString('ide', 'indPres', '1'));
  Ide.UFIni   := AINIRec.ReadString('ide', 'UFIni', '');
  Ide.cMunIni := AINIRec.ReadInteger('ide', 'cMunIni', 0);
  Ide.UFFim   := AINIRec.ReadString('ide', 'UFFim', '');
  Ide.cMunFim := AINIRec.ReadInteger('ide', 'cMunFim', 0);
  Ide.dhCont  := StringToDateTime(AINIRec.ReadString('ide', 'dhCont', '0'));
  Ide.xJust   := AINIRec.ReadString('ide', 'xJust', '');
end;

procedure TBPeIniReader.Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
var
  OK: Boolean;
begin
  //
  // Seção [emit] Emitente do BP-e
  //
  Emit.CNPJ  := AINIRec.ReadString('emit', 'CNPJ', '');
  Emit.IE    := AINIRec.ReadString('emit', 'IE', '');
  Emit.IEST  := AINIRec.ReadString('emit', 'IEST', '');
  Emit.xNome := AINIRec.ReadString('emit', 'xNome', '');
  Emit.xFant := AINIRec.ReadString('emit', 'xFant', '');
  Emit.IM    := AINIRec.ReadString('emit', 'IM', '');
  Emit.CNAE  := AINIRec.ReadString('emit', 'CNAE', '');
  Emit.CRT   := StrToCRT(OK, AINIRec.ReadString('emit', 'CRT', '3'));
  Emit.TAR   := AINIRec.ReadString('emit', 'TAR', '');

  Emit.enderEmit.xLgr    := AINIRec.ReadString('emit', 'xLgr', '');
  Emit.enderEmit.nro     := AINIRec.ReadString('emit', 'nro', '');
  Emit.enderEmit.xCpl    := AINIRec.ReadString('emit', 'xCpl', '');
  Emit.enderEmit.xBairro := AINIRec.ReadString('emit', 'xBairro', '');
  Emit.enderEmit.cMun    := AINIRec.ReadInteger('emit', 'cMun', 0);
  Emit.enderEmit.xMun    := AINIRec.ReadString('emit', 'xMun', '');
  Emit.enderEmit.CEP     := AINIRec.ReadInteger('emit', 'CEP', 0);
  Emit.enderEmit.UF      := AINIRec.ReadString('emit', 'UF', '');
  Emit.enderEmit.fone    := AINIRec.ReadString('emit', 'fone', '');
  Emit.enderEmit.Email   := AINIRec.ReadString('emit', 'Email', '');
end;

procedure TBPeIniReader.Ler_Comprador(AINIRec: TMemIniFile; Comp: TComp);
begin
  //
  // Seção [comp] Comprador
  //
  Comp.xNome         := AINIRec.ReadString('comp', 'xNome', '');
  Comp.CNPJCPF       := AINIRec.ReadString('comp', 'CNPJCPF', '');
  Comp.idEstrangeiro := AINIRec.ReadString('comp', 'idEstrangeiro', '');
  Comp.IE            := AINIRec.ReadString('comp', 'IE', '');

  Comp.enderComp.xLgr    := AINIRec.ReadString('comp', 'xLgr', '');
  Comp.enderComp.nro     := AINIRec.ReadString('comp', 'nro', '');
  Comp.enderComp.xCpl    := AINIRec.ReadString('comp', 'xCpl', '');
  Comp.enderComp.xBairro := AINIRec.ReadString('comp', 'xBairro', '');
  Comp.enderComp.cMun    := AINIRec.ReadInteger('comp', 'cMun', 0);
  Comp.enderComp.xMun    := AINIRec.ReadString('comp', 'xMun', '');
  Comp.enderComp.CEP     := AINIRec.ReadInteger('comp', 'CEP', 0);
  Comp.enderComp.UF      := AINIRec.ReadString('comp', 'UF', '');
  Comp.EnderComp.cPais   := AINIRec.ReadInteger('comp', 'cPais', 0);
  Comp.EnderComp.xPais   := AINIRec.ReadString('comp', 'xPais', '');
  Comp.enderComp.fone    := AINIRec.ReadString('comp', 'fone', '');
  Comp.enderComp.Email   := AINIRec.ReadString('comp', 'Email', '');
end;

procedure TBPeIniReader.Ler_Agencia(AINIRec: TMemIniFile; Agencia: TAgencia);
begin
  //
  // Seção [Agencia] Agência que comercializou o BP-e
  //
  Agencia.xNome := AINIRec.ReadString('Agencia', 'xNome', '');
  Agencia.CNPJ  := AINIRec.ReadString('Agencia', 'CNPJ', '');

  Agencia.enderAgencia.xLgr    := AINIRec.ReadString('Agencia', 'xLgr', '');
  Agencia.enderAgencia.nro     := AINIRec.ReadString('Agencia', 'nro', '');
  Agencia.enderAgencia.xCpl    := AINIRec.ReadString('Agencia', 'xCpl', '');
  Agencia.enderAgencia.xBairro := AINIRec.ReadString('Agencia', 'xBairro', '');
  Agencia.enderAgencia.cMun    := AINIRec.ReadInteger('Agencia', 'cMun', 0);
  Agencia.enderAgencia.xMun    := AINIRec.ReadString('Agencia', 'xMun', '');
  Agencia.enderAgencia.CEP     := AINIRec.ReadInteger('Agencia', 'CEP', 0);
  Agencia.enderAgencia.UF      := AINIRec.ReadString('Agencia', 'UF', '');
  Agencia.enderAgencia.cPais   := AINIRec.ReadInteger('Agencia', 'cPais', 0);
  Agencia.enderAgencia.xPais   := AINIRec.ReadString('Agencia', 'xPais', '');
  Agencia.enderAgencia.fone    := AINIRec.ReadString('Agencia', 'fone', '');
  Agencia.enderAgencia.Email   := AINIRec.ReadString('Agencia', 'Email', '');
end;

procedure TBPeIniReader.Ler_InfBPeSub(AINIRec: TMemIniFile;
  infBPeSub: TInfBPeSub);
var
  OK: Boolean;
begin
  //
  // Seção [infBPeSub] Informações do BP-e Substituido
  //
  if AINIRec.ReadString('infBPeSub', 'chBPe', '') <> '' then
  begin
    infBPeSub.chBPe := AINIRec.ReadString('infBPeSub', 'chBPe', '');
    infBPeSub.tpSub := StrTotpSubstituicao(OK, AINIRec.ReadString('infBPeSub', 'tpSub', '1'));
  end;
end;

procedure TBPeIniReader.Ler_InfPassagem(AINIRec: TMemIniFile;
  infPassagem: TInfPassagem);
begin
  //
  // Seção [infPassagem] Informações da Passagem
  //
  infPassagem.cLocOrig   := AINIRec.ReadString('infPassagem', 'cLocOrig', '');
  infPassagem.xLocOrig   := AINIRec.ReadString('infPassagem', 'xLocOrig', '');
  infPassagem.cLocDest   := AINIRec.ReadString('infPassagem', 'cLocDest', '');
  infPassagem.xLocDest   := AINIRec.ReadString('infPassagem', 'xLocDest', '');
  infPassagem.dhEmb      := StringToDateTime(AINIRec.ReadString('infPassagem', 'dhEmb', '0'));
  infPassagem.dhValidade := StringToDateTime(AINIRec.ReadString('infPassagem', 'dhValidade', '0'));
end;

procedure TBPeIniReader.Ler_InfPassageiro(AINIRec: TMemIniFile;
  infPassageiro: TInfPassageiro);
var
  OK: Boolean;
begin
  //
  // Seção [infPassageiro] Informações do Passageiro
  //
  infPassageiro.xNome := AINIRec.ReadString('infPassageiro', 'xNome', '');
  infPassageiro.CPF   := AINIRec.ReadString('infPassageiro', 'CPF', '');
  infPassageiro.tpDoc := StrTotpDocumento(OK, AINIRec.ReadString('infPassageiro', 'tpDoc', '1'));
  infPassageiro.nDoc  := AINIRec.ReadString('infPassageiro', 'nDoc', '');
  infPassageiro.xDoc  := AINIRec.ReadString('infPassageiro', 'xDoc', '');
  infPassageiro.dNasc := StringToDateTime(AINIRec.ReadString('infPassageiro', 'dNasc', '0'));
  infPassageiro.fone  := AINIRec.ReadString('infPassageiro', 'fone', '');
  infPassageiro.Email := AINIRec.ReadString('infPassageiro', 'Email', '');

end;

procedure TBPeIniReader.Ler_InfViagem(AINIRec: TMemIniFile;
  infViagem: TInfViagemCollection);
var
  I: Integer;
  sSecao, sFim: String;
  OK: Boolean;
  IteminfViagem: TInfViagemCollectionItem;
begin
  //
  // Seção [infViagemxxx] Informações da Viagem
  //
  I := 1;
  while true do
  begin
    sSecao := 'infViagem' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'cPercurso', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    IteminfViagem := infViagem.New;

    IteminfViagem.cPercurso    := sFim;
    IteminfViagem.xPercurso    := AINIRec.ReadString(sSecao, 'xPercurso', '');
    IteminfViagem.tpViagem     := StrTotpViagem(OK, AINIRec.ReadString(sSecao, 'tpViagem', '00'));
    IteminfViagem.tpServ       := StrTotpServico(OK, AINIRec.ReadString(sSecao, 'tpServ', '1'));
    IteminfViagem.tpAcomodacao := StrTotpAcomodacao(OK, AINIRec.ReadString(sSecao, 'tpAcomodacao', '1'));
    IteminfViagem.tpTrecho     := StrTotpTrecho(OK, AINIRec.ReadString(sSecao, 'tpTrecho', '1'));
    IteminfViagem.dhViagem     := StringToDateTime(AINIRec.ReadString(sSecao, 'dhViagem', '0'));
    IteminfViagem.dhConexao    := StringToDateTime(AINIRec.ReadString(sSecao, 'dhConexao', '0'));
    IteminfViagem.Prefixo      := AINIRec.ReadString(sSecao, 'Prefixo', '');
    IteminfViagem.Poltrona     := AINIRec.ReadInteger(sSecao, 'Poltrona', 0);
    IteminfViagem.Plataforma   := AINIRec.ReadString(sSecao, 'Plataforma', '');

    //
    // Informações da Travessia
    //
    if AINIRec.ReadString(sSecao, 'tpVeiculo', '') <> '' then
    begin
      IteminfViagem.infTravessia.tpVeiculo  := StrTotpVeiculo(OK, AINIRec.ReadString(sSecao, 'tpVeiculo', '01'));
      IteminfViagem.infTravessia.sitVeiculo := StrToSitVeiculo(OK, AINIRec.ReadString(sSecao, 'sitVeiculo', '01'));
    end;

    Inc(I);
  end;
end;

procedure TBPeIniReader.Ler_InfValorBPe(AINIRec: TMemIniFile;
  infValorBPe: TinfValorBPe);
var
  OK: Boolean;
begin
  //
  // Seção [infValorBPe] Informações dos Valores do BP-e
  //
  infValorBPe.vBP        := StringToFloatDef(AINIRec.ReadString('infValorBPe', 'vBP', ''), 0);
  infValorBPe.vDesconto  := StringToFloatDef(AINIRec.ReadString('infValorBPe', 'vDesconto', ''), 0);
  infValorBPe.vPgto      := StringToFloatDef(AINIRec.ReadString('infValorBPe', 'vPgto', ''), 0);
  infValorBPe.vTroco     := StringToFloatDef(AINIRec.ReadString('infValorBPe', 'vTroco', ''), 0);
  infValorBPe.tpDesconto := StrTotpDesconto(OK, AINIRec.ReadString('infValorBPe', 'tpDesconto', '01'));
  infValorBPe.xDesconto  := AINIRec.ReadString('infValorBPe', 'xDesconto', '');
  infValorBPe.cDesconto  := AINIRec.ReadString('infValorBPe', 'cDesconto', '');
end;

procedure TBPeIniReader.Ler_Componentes(AINIRec: TMemIniFile;
  Comp: TCompCollection);
var
  I: Integer;
  sSecao, sFim: String;
  OK: Boolean;
  ItemComp: TCompCollectionItem;
begin
  //
  // Seção [Compxxx] Componentes do Valor do BPe
  //
  I := 1;
  while true do
  begin
    sSecao := 'Comp' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'tpComp', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    ItemComp := Comp.New;
    ItemComp.tpComp := StrTotpComponente(OK, sFim);
    ItemComp.vComp  := StringToFloatDef(AINIRec.ReadString(sSecao, 'vComp', ''), 0);

    Inc(I);
  end;
end;

procedure TBPeIniReader.Ler_Impostos(AINIRec: TMemIniFile; Imp: TImp);
var
  sSecao, sFim: String;
begin
  sSecao := 'ICMS';
  sFim   := AINIRec.ReadString(sSecao, 'CST', 'FIM');

  if (sFim <> 'FIM') then
  begin
    Imp.vTotTrib   := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotTrib', ''), 0);
    Imp.infAdFisco := AINIRec.ReadString(sSecao, 'infAdFisco', '');

    Ler_ICMS(AINIRec, Imp.ICMS);
    Ler_ICMSUFFim(AINIRec, Imp.ICMSUFFim);
  end;
end;

procedure TBPeIniReader.Ler_ICMS(AINIRec: TMemIniFile; ICMS: TICMS);
var
  sSecao, sFim: String;
  OK: Boolean;
begin
  //
  // Seção [ICMS] Informacoes relativas aos Impostos
  //
  sSecao := 'ICMS';
  sFim   := AINIRec.ReadString(sSecao, 'CST', 'FIM');

  if (sFim <> 'FIM') then
  begin
    ICMS.CST    := StrToCSTICMS(OK, sFim);
    ICMS.pRedBC := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedBC', ''), 0);
    ICMS.vBC    := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBC', ''), 0);
    ICMS.pICMS  := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMS', ''), 0);
    ICMS.vICMS  := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMS', ''), 0);
    ICMS.vCred  := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCred', ''), 0);

    ICMS.pRedBCOutraUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedBCOutraUF', ''), 0);
    ICMS.vBCOutraUF    := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCOutraUF', ''), 0);
    ICMS.pICMSOutraUF  := StringToFloatDef(AINIRec.ReadString(sSecao, 'pICMSOutraUF', ''), 0);
    ICMS.vICMSOutraUF  := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSOutraUF', ''), 0);

    ICMS.vICMSDeson := StringToFloatDef(AINIRec.ReadString(sSecao, 'vICMSDeson', ''), 0);
    ICMS.cBenef     := AINIRec.ReadString(sSecao, 'cBenef', '');
  end;
end;

procedure TBPeIniReader.Ler_ICMSUFFim(AINIRec: TMemIniFile;
  ICMSUFFim: TICMSUFFim);
begin
  //
  // Seção [ICMSUFFim] Informacoes relativas aos Impostos
  //
  if StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0) <> 0 then
  begin
    ICMSUFFim.vBCUFFim       := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vBCUFFim', ''), 0);
    ICMSUFFim.pFCPUFFim      := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pFCPUFFim', ''), 0);
    ICMSUFFim.pICMSUFFim     := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSUFFim', ''), 0);
    ICMSUFFim.pICMSInter     := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSInter', ''), 0);
    ICMSUFFim.pICMSInterPart := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0);
    ICMSUFFim.vFCPUFFim      := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vFCPUFFim', ''), 0);
    ICMSUFFim.vICMSUFFim     := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vICMSUFFim', ''), 0);
    ICMSUFFim.vICMSUFIni     := StringToFloatDef(AINIRec.ReadString('ICMSUFFim', 'vICMSUFIni', ''), 0);
  end;
end;

procedure TBPeIniReader.Ler_Pagamentos(AINIRec: TMemIniFile;
  Pag: TpagCollection);
var
  I: Integer;
  sSecao, sFim: String;
  OK: Boolean;
  ItemPag: TpagCollectionItem;
begin
  //
  // Seção [Pagxx] Dados do Pagamento 01-10
  //
  I := 1 ;
  while true do
  begin
    sSecao := 'pag'+IntToStrZero(I,2) ;
    sFim   := AINIRec.ReadString(sSecao, 'tpag', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break ;

    ItemPag := Pag.New;

    ItemPag.tPag    := StrToFormaPagamentoBPe(OK, sFim);
    ItemPag.xPag    := AINIRec.ReadString(sSecao, 'xPag', '');
    ItemPag.nDocPag := AINIRec.ReadString(sSecao, 'nDocPag', '');
    ItemPag.vPag    := StringToFloatDef(AINIRec.ReadString(sSecao, 'vPag', ''), 0);

    ItemPag.tpIntegra := StrTotpIntegra(Ok, AINIRec.ReadString(sSecao, 'tpIntegra', ''));
    ItemPag.CNPJ      := AINIRec.ReadString(sSecao, 'CNPJ', '');
    ItemPag.tBand     := StrToBandeiraCard(OK, AINIRec.ReadString(sSecao, 'tBand', '99'));
    ItemPag.xBand     := AINIRec.ReadString(sSecao, 'xBand', '');
    ItemPag.cAut      := AINIRec.ReadString(sSecao, 'cAut', '');
    ItemPag.nsuTrans  := AINIRec.ReadString(sSecao, 'nsuTrans', '');
    ItemPag.nsuHost   := AINIRec.ReadString(sSecao, 'nsuHost', '');
    ItemPag.nParcelas := AINIRec.ReadInteger(sSecao, 'nParcelas', 1);
    ItemPag.infAdCard := AINIRec.ReadString(sSecao, 'infAdCard', '');

    Inc(I);
  end;
end;

procedure TBPeIniReader.Ler_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  I: Integer;
  sSecao, sFim: String;
begin
  //
  // Seção [auxXMLxx] Autorizados para Download do XML do BPe 01-10
  //
  I := 1 ;
  while true do
  begin
    sSecao := 'autXML' + IntToStrZero(I,2) ;
    sFim   := OnlyNumber(AINIRec.ReadString(sSecao, 'CNPJCPF', 'FIM'));
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break ;

    autXML.New.CNPJCPF := sFim;

    Inc(I);
  end;
end;

procedure TBPeIniReader.Ler_InfAdic(AINIRec: TMemIniFile; InfAdic: TInfAdic);
begin
  //
  // Seção [infAdic] Informações Adicionais
  //
  InfAdic.infAdFisco := AINIRec.ReadString('infAdic', 'infAdFisco', '');
  InfAdic.infCpl     := AINIRec.ReadString('infAdic', 'infCpl', '');
end;

procedure TBPeIniReader.Ler_InfRespTec(AINIRec: TMemIniFile;
  infRespTec: TinfRespTec);
var
  sSecao: string;
begin
  //
  // Seção [infRespTec] Informações do Responsável Técnico
  //
  sSecao := 'infRespTec';
  if AINIRec.SectionExists(sSecao) then
  begin
    infRespTec.CNPJ     := AINIRec.ReadString(sSecao, 'CNPJ', '');
    infRespTec.xContato := AINIRec.ReadString(sSecao, 'xContato', '');
    infRespTec.email    := AINIRec.ReadString(sSecao, 'email', '');
    infRespTec.fone     := AINIRec.ReadString(sSecao, 'fone', '');
  end;
end;

end.
