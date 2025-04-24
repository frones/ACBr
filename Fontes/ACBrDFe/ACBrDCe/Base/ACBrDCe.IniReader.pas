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

unit ACBrDCe.IniReader;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrDCe.Classes,
  ACBrDCe.Conversao;

type
  { TDCeIniReader }

  TDCeIniReader = class
  private
    FDCe: TDCe;
    FVersaoDF: TVersaoDCe;
    FAmbiente: Integer;
    FtpEmis: Integer;

    procedure Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Ler_Fisco(AINIRec: TMemIniFile; Fisco: TFisco);
    procedure Ler_Marketplace(AINIRec: TMemIniFile; Marketplace: TMarketplace);
    procedure Ler_Transportadora(AINIRec: TMemIniFile; Transportadora: TTransportadora);
    procedure Ler_ECT(AINIRec: TMemIniFile; ECT: TECT);
    procedure Ler_Destinatario(AINIRec: TMemIniFile; Dest: Tdest);
    procedure Ler_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Ler_Produtos(AINIRec: TMemIniFile; det: TDetCollection);
    procedure Ler_Total(AINIRec: TMemIniFile; total: Ttotal);
    procedure Ler_Transporte(AINIRec: TMemIniFile; transp: Ttransp);
    procedure Ler_InfAdicionais(AINIRec: TMemIniFile; infAdic: TinfAdic);
    procedure Ler_ObsFisco(AINIRec: TMemIniFile; obsFisco: TobsFiscoCollection);
    procedure Ler_ObsMarketplace(AINIRec: TMemIniFile; obsMarketplace: TobsMarketplaceCollection);
    procedure Ler_ObsEmitente(AINIRec: TMemIniFile; obsEmit: TobsEmitCollection);
    procedure Ler_ObsECT(AINIRec: TMemIniFile; obsECT: TobsECTCollection);
  public
    constructor Create(AOwner: TDCe); reintroduce;

    function LerIni(const AIniString: string): Boolean;

    property DCe: TDCe read FDCe write FDCe;
    property VersaoDF: TVersaoDCe read FVersaoDF write FVersaoDF;
    property Ambiente: Integer read FAmbiente write FAmbiente;
    property tpEmis: Integer read FtpEmis write FtpEmis;
  end;

implementation

uses
  ACBrXmlBase,
  ACBrDCe,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

{ TDCeIniReader }

constructor TDCeIniReader.Create(AOwner: TDCe);
begin
  inherited Create;

  FDCe := AOwner;
end;

function TDCeIniReader.LerIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    FDCe.infDCe.versao := StringToFloatDef(INIRec.ReadString('infDCe', 'versao', VersaoDCeToStr(VersaoDF)),0);

    Ler_Identificacao(INIRec, FDCe.Ide);
    Ler_Emitente(INIRec, FDCe.Emit);

    FDCe.ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigoUF(FDCe.Emit.enderEmit.UF));

    Ler_Fisco(INIRec, FDCe.Fisco);
    Ler_Marketplace(INIRec, FDCe.Marketplace);
    Ler_Transportadora(INIRec, FDCe.Transportadora);
    Ler_ECT(INIRec, FDCe.ECT);
    Ler_Destinatario(INIRec, FDCe.Dest);
    Ler_AutorizadosXml(INIRec, FDCe.autXML);
    Ler_Produtos(INIRec, FDCe.det);
    Ler_Total(INIRec, FDCe.total);
    Ler_Transporte(INIRec, FDCe.transp);
    Ler_InfAdicionais(INIRec, FDCe.infAdic);
    Ler_ObsFisco(INIRec, FDCe.obsFisco);
    Ler_ObsMarketplace(INIRec, FDCe.obsMarketplace);
    Ler_ObsEmitente(INIRec, FDCe.obsEmit);
    Ler_ObsECT(INIRec, FDCe.obsECT);
  finally
    INIRec.Free;
  end;
end;

procedure TDCeIniReader.Ler_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  sSecao: string;
  OK: Boolean;
begin
  sSecao := 'ide';
  Ide.tpAmb := StrToTipoAmbiente(OK, AINIRec.ReadString(sSecao, 'tpAmb', IntToStr(Ambiente)));
  Ide.modelo := AINIRec.ReadInteger(sSecao, 'Modelo', 99);
  Ide.serie := AINIRec.ReadInteger(sSecao, 'Serie', 1);
  Ide.nDC := AINIRec.ReadInteger(sSecao, 'nDC', 0);
  Ide.cDC := AINIRec.ReadInteger(sSecao, 'cDC', 0);
  Ide.dhEmi := StringToDateTime(AINIRec.ReadString(sSecao, 'dhEmi', '0'));
  Ide.tpEmis := StrToTipoEmissao(OK, AINIRec.ReadString(sSecao, 'tpEmis', IntToStr(tpEmis)));
  Ide.tpEmit  := StrToEmitenteDCe(AINIRec.ReadString(sSecao, 'tpEmit', '1'));
  Ide.nSiteAutoriz := StrToSiteAutorizator(AINIRec.ReadString(sSecao, 'nSiteAutoriz', '0'));
  Ide.verProc := AINIRec.ReadString(sSecao, 'verProc', 'ACBrNFCom');
end;

procedure TDCeIniReader.Ler_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
var
  sSecao: string;
begin
  sSecao := 'emit';
  Emit.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
  Emit.idOutros := AINIRec.ReadString(sSecao, 'idOutros', '');
  Emit.xNome := AINIRec.ReadString(sSecao, 'xNome', '');
  // Endereço do Emitente
  Emit.EnderEmit.xLgr := AINIRec.ReadString(sSecao, 'xLgr', '');
  Emit.EnderEmit.nro := AINIRec.ReadString(sSecao, 'nro', '');
  Emit.EnderEmit.xCpl := AINIRec.ReadString(sSecao, 'xCpl', '');
  Emit.EnderEmit.xBairro := AINIRec.ReadString(sSecao, 'xBairro', '');
  Emit.EnderEmit.cMun := AINIRec.ReadInteger(sSecao, 'cMun', 0);
  Emit.EnderEmit.xMun := AINIRec.ReadString(sSecao, 'xMun', '');
  Emit.EnderEmit.CEP := AINIRec.ReadInteger(sSecao, 'CEP', 0);
  Emit.EnderEmit.UF := AINIRec.ReadString(sSecao, 'UF', '');
  Emit.EnderEmit.fone := AINIRec.ReadString(sSecao, 'fone', '');
  Emit.EnderEmit.email := AINIRec.ReadString(sSecao, 'email', '');
end;

procedure TDCeIniReader.Ler_Fisco(AINIRec: TMemIniFile; Fisco: TFisco);
var
  sSecao: string;
begin
  sSecao := 'Fisco';
  Fisco.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  Fisco.xOrgao := AINIRec.ReadString(sSecao, 'xOrgao', '');
  Fisco.UF := AINIRec.ReadString(sSecao, 'UF', '');
end;

procedure TDCeIniReader.Ler_Marketplace(AINIRec: TMemIniFile;
  Marketplace: TMarketplace);
var
  sSecao: string;
begin
  sSecao := 'Marketplace';
  Marketplace.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  Marketplace.xNome := AINIRec.ReadString(sSecao, 'xNome', '');
  Marketplace.Site := AINIRec.ReadString(sSecao, 'Site', '');
end;

procedure TDCeIniReader.Ler_Transportadora(AINIRec: TMemIniFile;
  Transportadora: TTransportadora);
var
  sSecao: string;
begin
  sSecao := 'Transportadora';
  Transportadora.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  Transportadora.xNome := AINIRec.ReadString(sSecao, 'xNome', '');
end;

procedure TDCeIniReader.Ler_ECT(AINIRec: TMemIniFile; ECT: TECT);
var
  sSecao: string;
begin
  sSecao := 'ECT';
  ECT.CNPJ := AINIRec.ReadString(sSecao, 'CNPJ', '');
  ECT.xNome := AINIRec.ReadString(sSecao, 'xNome', '');
end;

procedure TDCeIniReader.Ler_Destinatario(AINIRec: TMemIniFile; Dest: Tdest);
var
  sSecao: string;
begin
  sSecao := 'dest';
  Dest.xNome := AINIRec.ReadString(sSecao, 'xNome', '');
  Dest.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
  Dest.idOutros := AINIRec.ReadString(sSecao, 'idOutros','');
  // Endereço do Destinatario
  Dest.EnderDest.xLgr := AINIRec.ReadString(sSecao, 'xLgr', '');
  Dest.EnderDest.nro := AINIRec.ReadString(sSecao, 'nro', '');
  Dest.EnderDest.xCpl := AINIRec.ReadString(sSecao, 'xCpl', '');
  Dest.EnderDest.xBairro := AINIRec.ReadString(sSecao, 'xBairro', '');
  Dest.EnderDest.cMun := AINIRec.ReadInteger(sSecao, 'cMun', 0);
  Dest.EnderDest.xMun := AINIRec.ReadString(sSecao, 'xMun', '');
  Dest.EnderDest.CEP := AINIRec.ReadInteger(sSecao, 'CEP', 0);
  Dest.EnderDest.UF := AINIRec.ReadString(sSecao, 'UF', '');
  Dest.EnderDest.fone := AINIRec.ReadString(sSecao, 'fone', '');
  Dest.EnderDest.email := AINIRec.ReadString(sSecao, 'email', '');
end;

procedure TDCeIniReader.Ler_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  I: Integer;
  sSecao, sFim: String;
begin
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

procedure TDCeIniReader.Ler_Produtos(AINIRec: TMemIniFile; det: TDetCollection);
var
  i: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'prod' + IntToStrZero(I, 3);
    sFim   := AINIRec.ReadString(sSecao, 'xProd', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with det.New do
    begin
      Prod.nItem :=  I;
      Prod.xProd := sFim;

      Prod.NCM := AINIRec.ReadString(sSecao, 'NCM', '');
      Prod.qCom := StringToFloatDef(AINIRec.ReadString(sSecao, 'qCom', ''), 0);
      Prod.vUnCom := StringToFloatDef(AINIRec.ReadString(sSecao, 'vUnCom', ''), 0);
      Prod.vProd := StringToFloatDef(AINIRec.ReadString(sSecao, 'vProd', ''), 0);

      infAdProd := AINIRec.ReadString(sSecao, 'infAdProd', '');
    end;

    Inc(I);
  end;
end;

procedure TDCeIniReader.Ler_Total(AINIRec: TMemIniFile; total: Ttotal);
var
  sSecao: string;
begin
  sSecao := 'total';
  total.vDC := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDC', ''), 0);
end;

procedure TDCeIniReader.Ler_Transporte(AINIRec: TMemIniFile; transp: Ttransp);
var
  sSecao: string;
begin
  sSecao := 'transp';
  transp.modTrans := StrToModTrans(AINIRec.ReadString(sSecao, 'modTrans', '0'));
  transp.CNPJTransp := AINIRec.ReadString(sSecao, 'CNPJTransp', '');
end;

procedure TDCeIniReader.Ler_InfAdicionais(AINIRec: TMemIniFile;
  infAdic: TinfAdic);
var
  sSecao: string;
begin
  sSecao := 'infAdic';
  infAdic.infAdFisco := AINIRec.ReadString(sSecao, 'infAdFisco', '');
  infAdic.infCpl := AINIRec.ReadString(sSecao, 'infCpl', '');
  infAdic.infAdMarketplace := AINIRec.ReadString(sSecao, 'infAdMarketplace', '');
  infAdic.infAdTransp := AINIRec.ReadString(sSecao, 'infAdTransp', '');
  infAdic.infAdECT := AINIRec.ReadString(sSecao, 'infAdECT', '');
end;

procedure TDCeIniReader.Ler_ObsFisco(AINIRec: TMemIniFile;
  obsFisco: TobsFiscoCollection);
var
  i: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'obsFisco' + IntToStrZero(I, 2);
    sFim   := AINIRec.ReadString(sSecao, 'xCampo', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with obsFisco.New do
    begin
      xCampo := sFim;
      xTexto := AINIRec.ReadString(sSecao, 'xTexto', '');
    end;

    Inc(I);
  end;
end;

procedure TDCeIniReader.Ler_ObsMarketplace(AINIRec: TMemIniFile;
  obsMarketplace: TobsMarketplaceCollection);
var
  i: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'obsMarketplace' + IntToStrZero(I, 2);
    sFim   := AINIRec.ReadString(sSecao, 'xCampo', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with obsMarketplace.New do
    begin
      xCampo := sFim;
      xTexto := AINIRec.ReadString(sSecao, 'xTexto', '');
    end;

    Inc(I);
  end;
end;

procedure TDCeIniReader.Ler_ObsEmitente(AINIRec: TMemIniFile;
  obsEmit: TobsEmitCollection);
var
  i: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'obsEmit' + IntToStrZero(I, 2);
    sFim   := AINIRec.ReadString(sSecao, 'xCampo', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with obsEmit.New do
    begin
      xCampo := sFim;
      xTexto := AINIRec.ReadString(sSecao, 'xTexto', '');
    end;

    Inc(I);
  end;
end;

procedure TDCeIniReader.Ler_ObsECT(AINIRec: TMemIniFile;
  obsECT: TobsECTCollection);
var
  i: Integer;
  sSecao, sFim: string;
begin
  I := 1;
  while true do
  begin
    sSecao := 'obsECT' + IntToStrZero(I, 2);
    sFim   := AINIRec.ReadString(sSecao, 'xCampo', 'FIM');
    if (sFim = 'FIM') or (Length(sFim) <= 0) then
      break;

    with obsECT.New do
    begin
      xCampo := sFim;
      xTexto := AINIRec.ReadString(sSecao, 'xTexto', '');
    end;

    Inc(I);
  end;
end;

end.
