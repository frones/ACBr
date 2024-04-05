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

unit ACBrONE.EnvRecepcaoLeitura;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrDFeConsts,
  pcnConversao,
  ACBrXmlBase,
  ACBrONE.Conversao;

type
  { TinfLeitura }

  TinfLeitura = class(TObject)
  private
    FtpTransm: TtpTransm;
    FdhTransm: TDateTime;
    FcUF: Integer;
    FdhPass: TDateTime;
    FCNPJOper: string;
    FxOper: string;
    FcEQP: string;
    FxEQP: string;
    Flatitude: Double;
    Flongitude: Double;
    FtpSentido: TtpSentido;
    Fplaca: string;
    FtpVeiculo: TtpVeiculo;
    Fvelocidade: Integer;
    Ffoto: string;
    FindiceConfianca: Integer;
    FpesoBrutoTotal: Integer;
    FnroEixos: Integer;
    FNSULeitura: string;
    FtpLeitura: TtpLeitura;
    FtpEQP: TtpEQP;
    FxRefCompl: string;

  public
    property tpTransm: TtpTransm      read FtpTransm        write FtpTransm;
    property dhTransm: TDateTime      read FdhTransm        write FdhTransm;
    property cUF: Integer             read FcUF             write FcUF;
    property dhPass: TDateTime        read FdhPass          write FdhPass;
    property CNPJOper: string         read FCNPJOper        write FCNPJOper;
    property xOper: string            read FxOper           write FxOper;
    property cEQP: string             read FcEQP            write FcEQP;
    property xEQP: string             read FxEQP            write FxEQP;
    property latitude: Double         read Flatitude        write Flatitude;
    property longitude: Double        read Flongitude       write Flongitude;
    property tpSentido: TtpSentido    read FtpSentido       write FtpSentido;
    property placa: string            read Fplaca           write Fplaca;
    property tpVeiculo: TtpVeiculo    read FtpVeiculo       write FtpVeiculo;
    property velocidade: Integer      read Fvelocidade      write Fvelocidade;
    property foto: string             read Ffoto            write Ffoto;
    property indiceConfianca: Integer read FindiceConfianca write FindiceConfianca;
    property pesoBrutoTotal: Integer  read FpesoBrutoTotal  write FpesoBrutoTotal;
    property nroEixos: Integer        read FnroEixos        write FnroEixos;
    property NSULeitura: string       read FNSULeitura      write FNSULeitura;
    property tpLeitura: TtpLeitura    read FtpLeitura       write FtpLeitura;
    property tpEQP: TtpEQP            read FtpEQP           write FtpEQP;
    property xRefCompl: string        read FxRefCompl       write FxRefCompl;
  end;

  { TRecepcaoLeitura }

  TRecepcaoLeitura = class(TObject)
  private
    FVersao: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FtpTransm: TtpTransm;
    FdhTransm: TDateTime;
    FinfLeitura: TinfLeitura;

  public
    constructor Create;
    destructor Destroy; override;

    function GerarXML: string;

    function LerXML(const CaminhoArquivo: string): Boolean;
    function LerXMLFromString(const AXML: string): Boolean;

    function LerFromIni(const AIniString: string): Boolean;
    function ObterNomeArquivo: string;

    property Versao: string           read FVersao     write FVersao;
    property tpAmb: TACBrTipoAmbiente read FtpAmb      write FtpAmb;
    property verAplic: string         read FverAplic   write FverAplic;
    property tpTransm: TtpTransm      read FtpTransm   write FtpTransm;
    property dhTransm: TDateTime      read FdhTransm   write FdhTransm;
    property infLeitura: TinfLeitura  read FinfLeitura write FinfLeitura;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base, ACBrUtil.DateTime, ACBrUtil.FilesIO,
  ACBrONE.Consts,
  ACBrONE.RetRecepcaoLeitura,
  ACBrDFeUtil;

{ TRecepcaoLeitura }

constructor TRecepcaoLeitura.Create;
begin
  inherited Create;

  FinfLeitura := TinfLeitura.Create;
end;

destructor TRecepcaoLeitura.Destroy;
begin
  FinfLeitura.Free;

  inherited;
end;

function TRecepcaoLeitura.GerarXML: string;
var
 sEQP, xAux: string;
begin
  sEQP := IntToStrZero(StrToInt64Def(infLeitura.cEQP, 0), 15);

  if sEQP <> '' then
    xAux := '<cEQP>' + sEQP + '</cEQP>'
  else
  begin
    xAux := '<latitude>' + FormatFloat('0.000000', infLeitura.latitude) + '</latitude>' +
            '<longitude>' + FormatFloat('0.000000', infLeitura.longitude) + '</longitude>' +
            '<tpSentido>' + TpSentidoToStr(infLeitura.tpSentido) + '</tpSentido>';

    if infLeitura.xEQP <> '' then
    begin
      xAux := xAux + '<xEQP>' + infLeitura.xEQP + '</xEQP>' +
                     '<tpEQP>' + TpEQPToStr(infLeitura.tpEQP) + '</tpEQP>' +
                     '<xRefCompl>' + infLeitura.xRefCompl + '</xRefCompl>';
    end;
  end;

  Result := '<oneRecepLeitura ' + NAME_SPACE_ONE + ' versao="' + Versao + '">' +
              '<tpAmb>' + TipoAmbienteToStr(tpAmb) + '</tpAmb>' +
              '<verAplic>' + verAplic + '</verAplic>' +
              '<tpTransm>' + tpTransmToStr(FtpTransm) + '</tpTransm>' +
              '<dhTransm>' +
                FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhTransm) +
                  GetUTC(CodigoUFparaUF(infLeitura.cUF), FdhTransm) +
              '</dhTransm>' +
              '<infLeitura>' +
                '<cUF>' + IntToStr(infLeitura.cUF) + '</cUF>' +
                '<dhPass>' +
                  FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', infLeitura.dhPass) +
                    GetUTC(CodigoUFparaUF(infLeitura.cUF), infLeitura.dhPass) +
                '</dhPass>' +
                '<CNPJOper>' + infLeitura.CNPJOper + '</CNPJOper>' +
                xAux +
                '<placa>' + infLeitura.placa + '</placa>' +
                '<tpVeiculo>' + tpVeiculoToStr(infLeitura.tpVeiculo) + '</tpVeiculo>' +
                '<velocidade>' + IntToStr(infLeitura.velocidade) + '</velocidade>' +
                '<foto>' + infLeitura.foto + '</foto>' +
                '<indiceConfianca>' + IntToStr(infLeitura.indiceConfianca) + '</indiceConfianca>' +
                '<pesoBrutoTotal>' + IntToStr(infLeitura.pesoBrutoTotal) + '</pesoBrutoTotal>' +
                '<nroEixos>' + IntToStr(infLeitura.nroEixos) + '</nroEixos>' +
              '</infLeitura>' +
            '</oneRecepLeitura>';
end;

function TRecepcaoLeitura.LerFromIni(const AIniString: string): Boolean;
var
  INIRec: TMemIniFile;
  sSecao: string;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    sSecao := 'RECEPCAOLEITURA';
    if INIRec.SectionExists(sSecao) then
    begin
      verAplic := INIRec.ReadString(sSecao, 'verAplic', '');
      tpTransm := StrTotpTransm(INIRec.ReadString(sSecao, 'tpTransm', '1'));
      dhTransm := StringToDateTime(INIRec.ReadString(sSecao, 'dhTransm', ''));

      with infLeitura do
      begin
        cUF             := INIRec.ReadInteger(sSecao, 'cUF', 0);
        dhPass          := StringToDateTime(INIRec.ReadString(sSecao, 'dhPass', ''));
        CNPJOper        := INIRec.ReadString(sSecao, 'CNPJOper', '');
        cEQP            := INIRec.ReadString(sSecao, 'cEQP', '');
        latitude        := INIRec.ReadFloat(sSecao, 'latitude', 0);
        longitude       := INIRec.ReadFloat(sSecao, 'longitude', 0);
        tpSentido       := StrTotpSentido(INIRec.ReadString(sSecao, 'tpSentido', 'E'));
        placa           := INIRec.ReadString(sSecao, 'placa', '');
        tpVeiculo       := StrTotpVeiculo(INIRec.ReadString(sSecao, 'tpVeiculo', 'E'));
        velocidade      := INIRec.ReadInteger(sSecao, 'velocidade', 0);
        foto            := INIRec.ReadString(sSecao, 'foto', '');
        indiceConfianca := INIRec.ReadInteger(sSecao, 'indiceConfianca', 0);
        pesoBrutoTotal  := INIRec.ReadInteger(sSecao, 'pesoBrutoTotal', 0);
        nroEixos        := INIRec.ReadInteger(sSecao, 'nroEixos', 0);
        xEQP            := INIRec.ReadString(sSecao, 'xEQP', '');
        tpEQP           := StrTotpEQP(INIRec.ReadString(sSecao, 'tpEQP', '1'));
        xRefCompl       := INIRec.ReadString(sSecao, 'xRefCompl', '');
      end;
    end;
  finally
    INIRec.Free;
  end;
end;

function TRecepcaoLeitura.LerXML(const CaminhoArquivo: string): Boolean;
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

function TRecepcaoLeitura.LerXMLFromString(const AXML: string): Boolean;
var
  RetRecepcaoLeitura : TRetRecepcaoLeitura;
begin
  RetRecepcaoLeitura := TRetRecepcaoLeitura.Create;

  try
    RetRecepcaoLeitura.XmlRetorno := AXML;
    Result := RetRecepcaoLeitura.LerXml;
    {
    verAplic  := RetManutencaoEQP.verAplic;
    tpMan     := RetManutencaoEQP.tpMan;
    dhReg     := RetManutencaoEQP.dhReg;
    CNPJOper  := RetManutencaoEQP.CNPJOper;
    cEQP      := RetManutencaoEQP.cEQP;
    xEQP      := RetManutencaoEQP.xEQP;
    cUF       := RetManutencaoEQP.cUF;
    tpSentido := RetManutencaoEQP.tpSentido;
    Latitude  := RetManutencaoEQP.Latitude;
    Longitude := RetManutencaoEQP.Longitude;
    tpEQP     := RetManutencaoEQP.tpEQP;
    }
  finally
    RetRecepcaoLeitura.Free;
  end;
end;

function TRecepcaoLeitura.ObterNomeArquivo: string;
begin
  Result := FormatDateTime('yyyymmddhhnnss', dhTransm) + '-lei.xml';
end;

end.
