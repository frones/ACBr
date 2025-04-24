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

unit ACBrDCe.IniWriter;

interface

uses
  Classes, SysUtils,
  IniFiles,
  ACBrDCe.Classes,
  ACBrDCe.Conversao;

type
  { TDCeIniWriter }

  TDCeIniWriter = class
  private
    FDCe: TDCe;

    procedure Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
    procedure Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
    procedure Gerar_Fisco(AINIRec: TMemIniFile; Fisco: TFisco);
    procedure Gerar_Marketplace(AINIRec: TMemIniFile; Marketplace: TMarketplace);
    procedure Gerar_Transportadora(AINIRec: TMemIniFile; Transportadora: TTransportadora);
    procedure Gerar_ECT(AINIRec: TMemIniFile; ECT: TECT);
    procedure Gerar_Destinatario(AINIRec: TMemIniFile; Dest: Tdest);
    procedure Gerar_AutorizadosXml(AINIRec: TMemIniFile; autXML: TautXMLCollection);
    procedure Gerar_Produtos(AINIRec: TMemIniFile; det: TDetCollection);
    procedure Gerar_Total(AINIRec: TMemIniFile; total: Ttotal);
    procedure Gerar_Transporte(AINIRec: TMemIniFile; transp: Ttransp);
    procedure Gerar_InfAdicionais(AINIRec: TMemIniFile; infAdic: TinfAdic);
    procedure Gerar_ObsFisco(AINIRec: TMemIniFile; obsFisco: TobsFiscoCollection);
    procedure Gerar_ObsMarketplace(AINIRec: TMemIniFile; obsMarketplace: TobsMarketplaceCollection);
    procedure Gerar_ObsEmitente(AINIRec: TMemIniFile; obsEmit: TobsEmitCollection);
    procedure Gerar_ObsECT(AINIRec: TMemIniFile; obsECT: TobsECTCollection);
  public
    constructor Create(AOwner: TDCe); reintroduce;

    function GravarIni: string;

    property DCe: TDCe read FDCe write FDCe;
  end;


implementation

uses
  ACBrXmlBase,
  ACBrDFeUtil,
  ACBrDCe,
  ACBrUtil.Base;

{ TDCeIniWriter }

constructor TDCeIniWriter.Create(AOwner: TDCe);
begin
  inherited Create;

  FDCe := AOwner;
end;

function TDCeIniWriter.GravarIni: string;
var
  INIRec: TMemIniFile;
  IniDCe: TStringList;
begin
  Result := '';

  if not ValidarChave(FDCe.infDCe.ID) then
    raise EACBrDCeException.Create('DCe Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    INIRec.WriteString('infDCe', 'ID', FDCe.infDCe.ID);
    INIRec.WriteString('infDCe', 'Versao', FloatToStr(FDCe.infDCe.Versao));

    Gerar_Identificacao(INIRec, FDCe.Ide);
    Gerar_Emitente(INIRec, FDCe.Emit);
    Gerar_Fisco(INIRec, FDCe.Fisco);
    Gerar_Marketplace(INIRec, FDCe.Marketplace);
    Gerar_Transportadora(INIRec, FDCe.Transportadora);
    Gerar_ECT(INIRec, FDCe.ECT);
    Gerar_Destinatario(INIRec, FDCe.Dest);
    Gerar_AutorizadosXml(INIRec, FDCe.autXML);
    Gerar_Produtos(INIRec, FDCe.det);
    Gerar_Total(INIRec, FDCe.total);
    Gerar_Transporte(INIRec, FDCe.transp);
    Gerar_InfAdicionais(INIRec, FDCe.infAdic);
    Gerar_ObsFisco(INIRec, FDCe.obsFisco);
    Gerar_ObsMarketplace(INIRec, FDCe.obsMarketplace);
    Gerar_ObsEmitente(INIRec, FDCe.obsEmit);
    Gerar_ObsECT(INIRec, FDCe.obsECT);

    IniDCe := TStringList.Create;
    try
      INIRec.GetStrings(IniDCe);
      Result := StringReplace(IniDCe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniDCe.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

procedure TDCeIniWriter.Gerar_Identificacao(AINIRec: TMemIniFile; Ide: TIde);
var
  sSecao: string;
begin
  sSecao := 'ide';
  AINIRec.WriteInteger(sSecao, 'cUF', Ide.cUF);
  AINIRec.WriteInteger(sSecao, 'cDC', Ide.cDC);
  AINIRec.WriteInteger(sSecao, 'Modelo', Ide.modelo);
  AINIRec.WriteInteger(sSecao, 'Serie', Ide.serie);
  AINIRec.WriteInteger(sSecao, 'nDC', Ide.nDC);
  AINIRec.WriteString(sSecao, 'dhEmi', DateTimeToStr(Ide.dhEmi));
  AINIRec.WriteString(sSecao, 'tpEmis', TipoEmissaoToStr(Ide.tpEmis));
  AINIRec.WriteString(sSecao, 'tpEmit', EmitenteDCeToStr(Ide.tpEmit));
  AINIRec.WriteString(sSecao, 'nSiteAutoriz', SiteAutorizadorToStr(Ide.nSiteAutoriz));
  AINIRec.WriteString(sSecao, 'tpAmb', TipoAmbienteToStr(Ide.tpAmb));
  AINIRec.WriteString(sSecao, 'verProc', Ide.verProc);
end;

procedure TDCeIniWriter.Gerar_Emitente(AINIRec: TMemIniFile; Emit: TEmit);
var
  sSecao: string;
begin
  sSecao := 'emit';
  AINIRec.WriteString(sSecao, 'CNPJCPF', Emit.CNPJCPF);
  AINIRec.WriteString(sSecao, 'idOutros', Emit.idOutros);
  AINIRec.WriteString(sSecao, 'xNome', Emit.xNome);
  // Endereço do Emitente
  AINIRec.WriteString(sSecao, 'xLgr', Emit.EnderEmit.xLgr);
  AINIRec.WriteString(sSecao, 'nro', Emit.EnderEmit.nro);
  AINIRec.WriteString(sSecao, 'xCpl', Emit.EnderEmit.xCpl);
  AINIRec.WriteString(sSecao, 'xBairro', Emit.EnderEmit.xBairro);
  AINIRec.WriteInteger(sSecao, 'cMun', Emit.EnderEmit.cMun);
  AINIRec.WriteString(sSecao, 'xMun', Emit.EnderEmit.xMun);
  AINIRec.WriteInteger(sSecao, 'CEP', Emit.EnderEmit.CEP);
  AINIRec.WriteString(sSecao, 'UF', Emit.EnderEmit.UF);
  AINIRec.WriteString(sSecao, 'fone', Emit.EnderEmit.fone);
  AINIRec.WriteString(sSecao, 'email', Emit.EnderEmit.email);
end;

procedure TDCeIniWriter.Gerar_Fisco(AINIRec: TMemIniFile; Fisco: TFisco);
var
  sSecao: string;
begin
  if Fisco.CNPJ <> '' then
  begin
    sSecao := 'Fisco';
    AINIRec.WriteString(sSecao, 'CNPJ', Fisco.CNPJ);
    AINIRec.WriteString(sSecao, 'xOrgao', Fisco.xOrgao);
    AINIRec.WriteString(sSecao, 'UF', Fisco.UF);
  end;
end;

procedure TDCeIniWriter.Gerar_Marketplace(AINIRec: TMemIniFile;
  Marketplace: TMarketplace);
var
  sSecao: string;
begin
  if Marketplace.CNPJ <> '' then
  begin
    sSecao := 'Marketplace';
    AINIRec.WriteString(sSecao, 'CNPJ', Marketplace.CNPJ);
    AINIRec.WriteString(sSecao, 'xNome', Marketplace.xNome);
    AINIRec.WriteString(sSecao, 'Site', Marketplace.Site);
  end;
end;

procedure TDCeIniWriter.Gerar_Transportadora(AINIRec: TMemIniFile;
  Transportadora: TTransportadora);
var
  sSecao: string;
begin
  if Transportadora.CNPJ <> '' then
  begin
    sSecao := 'Transportadora';
    AINIRec.WriteString(sSecao, 'CNPJ', Transportadora.CNPJ);
    AINIRec.WriteString(sSecao, 'xNome', Transportadora.xNome);
  end;
end;

procedure TDCeIniWriter.Gerar_ECT(AINIRec: TMemIniFile; ECT: TECT);
var
  sSecao: string;
begin
  if ECT.CNPJ <> '' then
  begin
    sSecao := 'ECT';
    AINIRec.WriteString(sSecao, 'CNPJ', ECT.CNPJ);
    AINIRec.WriteString(sSecao, 'xNome', ECT.xNome);
  end;
end;

procedure TDCeIniWriter.Gerar_Destinatario(AINIRec: TMemIniFile; Dest: Tdest);
var
  sSecao: string;
begin
  sSecao := 'dest';
  AINIRec.WriteString(sSecao, 'CNPJCPF', Dest.CNPJCPF);
  AINIRec.WriteString(sSecao, 'idOutros', Dest.idOutros);
  AINIRec.WriteString(sSecao, 'xNome', Dest.xNome);
  // Endereço do Destinatario
  AINIRec.WriteString(sSecao, 'xLgr', Dest.EnderDest.xLgr);
  AINIRec.WriteString(sSecao, 'nro', Dest.EnderDest.nro);
  AINIRec.WriteString(sSecao, 'xCpl', Dest.EnderDest.xCpl);
  AINIRec.WriteString(sSecao, 'xBairro', Dest.EnderDest.xBairro);
  AINIRec.WriteInteger(sSecao, 'cMun', Dest.EnderDest.cMun);
  AINIRec.WriteString(sSecao, 'xMun', Dest.EnderDest.xMun);
  AINIRec.WriteInteger(sSecao, 'CEP', Dest.EnderDest.CEP);
  AINIRec.WriteString(sSecao, 'UF', Dest.EnderDest.UF);
  AINIRec.WriteString(sSecao, 'fone', Dest.EnderDest.fone);
  AINIRec.WriteString(sSecao, 'email', Dest.EnderDest.email);
end;

procedure TDCeIniWriter.Gerar_AutorizadosXml(AINIRec: TMemIniFile;
  autXML: TautXMLCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to autXML.Count - 1 do
  begin
    sSecao := 'autXML' + IntToStrZero(I + 1, 2);
    with autXML[i] do
    begin
      AINIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
    end;
  end;
end;

procedure TDCeIniWriter.Gerar_Produtos(AINIRec: TMemIniFile;
  det: TDetCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to det.Count - 1 do
  begin
    sSecao := 'Prod' + IntToStrZero(I + 1, 3);
    with det[i] do
    begin
      AINIRec.WriteString(sSecao, 'xProd', Prod.xProd);
      AINIRec.WriteString(sSecao, 'NCM', Prod.NCM);
      AINIRec.WriteFloat(sSecao, 'qCom', Prod.qCom);
      AINIRec.WriteFloat(sSecao, 'vUnCom', Prod.vUnCom);
      AINIRec.WriteFloat(sSecao, 'vProd', Prod.vProd);
      AINIRec.WriteString(sSecao, 'infAdProd', infAdProd);
    end;
  end;
end;

procedure TDCeIniWriter.Gerar_Total(AINIRec: TMemIniFile; total: Ttotal);
var
  sSecao: string;
begin
  sSecao := 'total';
  AINIRec.WriteFloat(sSecao, 'vDC', total.vDC);
end;

procedure TDCeIniWriter.Gerar_Transporte(AINIRec: TMemIniFile; transp: Ttransp);
var
  sSecao: string;
begin
  sSecao := 'transp';
  AINIRec.WriteString(sSecao, 'modTrans', ModTransToStr(transp.modTrans));
  AINIRec.WriteString(sSecao, 'CNPJTransp', transp.CNPJTransp);
end;

procedure TDCeIniWriter.Gerar_InfAdicionais(AINIRec: TMemIniFile;
  infAdic: TinfAdic);
var
  sSecao: string;
begin
  sSecao := 'infAdic';
  AINIRec.WriteString(sSecao, 'infAdFisco', infAdic.infAdFisco);
  AINIRec.WriteString(sSecao, 'infCpl', infAdic.infCpl);
  AINIRec.WriteString(sSecao, 'infAdMarketplace', infAdic.infAdMarketplace);
  AINIRec.WriteString(sSecao, 'infAdTransp', infAdic.infAdTransp);
  AINIRec.WriteString(sSecao, 'infAdECT', infAdic.infAdECT);
end;

procedure TDCeIniWriter.Gerar_ObsFisco(AINIRec: TMemIniFile;
  obsFisco: TobsFiscoCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to obsFisco.Count - 1 do
  begin
    sSecao := 'obsFisco' + IntToStrZero(I + 1, 2);
    with obsFisco[i] do
    begin
      AINIRec.WriteString(sSecao, 'xCampo', xCampo);
      AINIRec.WriteString(sSecao, 'xTexto', xTexto);
    end;
  end;
end;

procedure TDCeIniWriter.Gerar_ObsMarketplace(AINIRec: TMemIniFile;
  obsMarketplace: TobsMarketplaceCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to obsMarketplace.Count - 1 do
  begin
    sSecao := 'obsMarketplace' + IntToStrZero(I + 1, 2);
    with obsMarketplace[i] do
    begin
      AINIRec.WriteString(sSecao, 'xCampo', xCampo);
      AINIRec.WriteString(sSecao, 'xTexto', xTexto);
    end;
  end;
end;

procedure TDCeIniWriter.Gerar_ObsEmitente(AINIRec: TMemIniFile;
  obsEmit: TobsEmitCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to obsEmit.Count - 1 do
  begin
    sSecao := 'obsEmit' + IntToStrZero(I + 1, 2);
    with obsEmit[i] do
    begin
      AINIRec.WriteString(sSecao, 'xCampo', xCampo);
      AINIRec.WriteString(sSecao, 'xTexto', xTexto);
    end;
  end;
end;

procedure TDCeIniWriter.Gerar_ObsECT(AINIRec: TMemIniFile;
  obsECT: TobsECTCollection);
var
  i: Integer;
  sSecao: string;
begin
  for i := 0 to obsECT.Count - 1 do
  begin
    sSecao := 'obsECT' + IntToStrZero(I + 1, 2);
    with obsECT[i] do
    begin
      AINIRec.WriteString(sSecao, 'xCampo', xCampo);
      AINIRec.WriteString(sSecao, 'xTexto', xTexto);
    end;
  end;
end;

end.
