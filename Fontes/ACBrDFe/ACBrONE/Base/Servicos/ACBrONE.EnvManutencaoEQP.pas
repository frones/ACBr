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

unit ACBrONE.EnvManutencaoEQP;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao,
  ACBrONE.Conversao;

type
  { TManutencaoEQP }

  TManutencaoEQP = class(TObject)
  private
    FVersao: string;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: string;
    FtpMan: TtpMan;
    FdhReg: TDateTime;
    FCNPJOper: string;
    FcEQP: string;
    FxEQP: string;
    FcUF: Integer;
    FtpSentido: TtpSentido;
    Flatitude: Double;
    Flongitude: Double;
    FtpEQP: TtpEQP;
    FxRefCompl: string;

  public
    function GerarXML: string;

    function LerXML(const CaminhoArquivo: string): Boolean;
    function LerXMLFromString(const AXML: string): Boolean;

    function LerFromIni(const AIniString: string): Boolean;
    function ObterNomeArquivo: string;

    property Versao: string          read FVersao    write FVersao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb     write FtpAmb;
    property verAplic: string        read FverAplic  write FverAplic;
    property tpMan: TtpMan           read FtpMan     write FtpMan;
    property dhReg: TDateTime        read FdhReg     write FdhReg;
    property CNPJOper: string        read FCNPJOper  write FCNPJOper;
    property cEQP: string            read FcEQP      write FcEQP;
    property xEQP: string            read FxEQP      write FxEQP;
    property cUF: Integer            read FcUF       write FcUF;
    property tpSentido: TtpSentido   read FtpSentido write FtpSentido;
    property latitude: Double        read Flatitude  write Flatitude;
    property longitude: Double       read Flongitude write Flongitude;
    property tpEQP: TtpEQP           read FtpEQP     write FtpEQP;
    property xRefCompl: string       read FxRefCompl write FxRefCompl;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrONE.RetManutencaoEQP,
  ACBrONE.Consts;

{ TManutencaoEQP }

function TManutencaoEQP.GerarXML: string;
var
 sEQP, xLat, xLon: string;
begin
  sEQP := IntToStrZero(StrToInt64Def(FcEQP, 0), 15);
  xLat := FormatFloat('0.000000', Flatitude);
  xLat := StringReplace(xLat, ',', '.', [rfReplaceAll]);
  xLon := FormatFloat('0.000000', Flongitude);
  xLon := StringReplace(xLon, ',', '.', [rfReplaceAll]);

  Result := '<oneManEQP ' + NAME_SPACE_ONE + ' versao="' + Versao + '">' +
              '<tpAmb>' + tpAmbToStr(tpAmb) + '</tpAmb>' +
              '<verAplic>' + verAplic + '</verAplic>' +
              '<tpMan>' + TpManToStr(FtpMan) + '</tpMan>' +
              '<dhReg>' +
                FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhReg) +
                  GetUTC(CodigoUFparaUF(FcUF), FdhReg) +
              '</dhReg>' +
              '<CNPJOper>' + FCNPJOper + '</CNPJOper>' +
              '<cEQP>' + sEQP + '</cEQP>' +
              '<xEQP>' + FxEQP + '</xEQP>' +
              '<cUF>' + IntToStr(FcUF) + '</cUF>' +
              '<tpSentido>' + TpSentidoToStr(FtpSentido) + '</tpSentido>' +
              '<latitude>' + xLat + '</latitude>' +
              '<longitude>' + xLon + '</longitude>' +
              '<tpEQP>' + TpEQPToStr(FtpEQP) + '</tpEQP>' +
              '<xRefCompl>' + xRefCompl + '</xRefCompl>' +
            '</oneManEQP>';
end;

function TManutencaoEQP.LerFromIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
  sSecao: string;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    sSecao := 'MANUTENCAO';
    if INIRec.SectionExists(sSecao) then
    begin
      verAplic  := INIRec.ReadString(sSecao, 'verAplic', '');
      tpMan     := StrToTpMan(INIRec.ReadString(sSecao, 'tpMan', '1'));
      dhReg     := StringToDateTime(INIRec.ReadString(sSecao, 'dhReg', ''));
      CNPJOper  := INIRec.ReadString(sSecao, 'CNPJOper', '');
      cEQP      := INIRec.ReadString(sSecao, 'cEQP', '');
      xEQP      := INIRec.ReadString(sSecao, 'xEQP', '');
      cUF       := INIRec.ReadInteger(sSecao, 'cUF', 0);
      tpSentido := StrTotpSentido(INIRec.ReadString(sSecao, 'tpSentido', 'E'));
      latitude  := INIRec.ReadFloat(sSecao, 'latitude', 0);
      longitude := INIRec.ReadFloat(sSecao, 'longitude', 0);
      tpEQP     := StrTotpEQP(INIRec.ReadString(sSecao, 'tpEQP', '1'));
    end;
  finally
    INIRec.Free;
  end;
end;

function TManutencaoEQP.LerXML(const CaminhoArquivo: string): Boolean;
var
  ArqXML: TStringList;
begin
  ArqXML := TStringList.Create;
  try
     ArqXML.LoadFromFile(CaminhoArquivo);
     Result := LerXMLFromString(ArqXML.Text);
  finally
     ArqXML.Free;
  end;
end;

function TManutencaoEQP.LerXMLFromString(const AXML: string): Boolean;
var
  RetManutencaoEQP : TRetManutencaoEQP;
begin
  RetManutencaoEQP := TRetManutencaoEQP.Create;

  try
    RetManutencaoEQP.XmlRetorno := AXML;
    Result := RetManutencaoEQP.LerXml;
    {
    verAplic  := RetManutencaoEQP.verAplic;
    tpMan     := RetManutencaoEQP.tpMan;
    dhReg     := RetManutencaoEQP.dhReg;
    CNPJOper  := RetManutencaoEQP.CNPJOper;
    cEQP      := RetManutencaoEQP.cEQP;
    xEQP      := RetManutencaoEQP.xEQP;
    cUF       := RetManutencaoEQP.cUF;
    tpSentido := RetManutencaoEQP.tpSentido;
    latitude  := RetManutencaoEQP.latitude;
    longitude := RetManutencaoEQP.longitude;
    tpEQP     := RetManutencaoEQP.tpEQP;
    }
  finally
    RetManutencaoEQP.Free;
  end;
end;

function TManutencaoEQP.ObterNomeArquivo: string;
begin
  Result := FormatDateTime('yyyymmddhhnnss', dhReg) + '-man.xml';
end;

end.
