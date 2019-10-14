{******************************************************************************}
{ Projeto: Componente ACBrONE                                                  }
{  Operador Nacional dos Estados - ONE                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2019                                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 14/10/2019: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnEnvRecepcaoLeitura;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnConversao, pcnGerador,
  pcnConversaoONE, pcnONEConsts;

type
  { TRecepcaoLeitura }

  TRecepcaoLeitura = class(TObject)
  private
    FGerador: TGerador;

    FVersao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FtpTransm: TtpTransm;
    FdhTransm: TDateTime;
    FcUF: Integer;
    FdhPass: TDateTime;
    FCNPJOper: String;
    FcEQP: String;
    Flatitude: Double;
    Flongitude: Double;
    FtpSentido: TtpSentido;
    Fplaca: String;
    FtpVeiculo: TtpVeiculo;
    Fvelocidade: Integer;
    Ffoto: String;
    FindiceConfianca: Integer;
    FpesoBrutoTotal: Integer;
    FnroEixos: Integer;

  public
    constructor Create;
    destructor Destroy; override;

    function GerarXML: Boolean;
    function LerXML(const CaminhoArquivo: String): Boolean;
    function LerXMLFromString(const AXML: String): Boolean;
    function LerFromIni(const AIniString: String): Boolean;
    function ObterNomeArquivo: String;

    property Gerador: TGerador        read FGerador         write FGerador;
    property Versao: String           read FVersao          write FVersao;
    property tpAmb: TpcnTipoAmbiente  read FtpAmb           write FtpAmb;
    property verAplic: String         read FverAplic        write FverAplic;
    property tpTransm: TtpTransm      read FtpTransm        write FtpTransm;
    property dhTransm: TDateTime      read FdhTransm        write FdhTransm;
    property cUF: Integer             read FcUF             write FcUF;
    property dhPass: TDateTime        read FdhPass          write FdhPass;
    property CNPJOper: String         read FCNPJOper        write FCNPJOper;
    property cEQP: String             read FcEQP            write FcEQP;
    property latitude: Double        read Flatitude        write Flatitude;
    property longitude: Double       read Flongitude       write Flongitude;
    property tpSentido: TtpSentido    read FtpSentido       write FtpSentido;
    property placa: String            read Fplaca           write Fplaca;
    property tpVeiculo: TtpVeiculo    read FtpVeiculo       write FtpVeiculo;
    property velocidade: Integer      read Fvelocidade      write Fvelocidade;
    property foto: String             read Ffoto            write Ffoto;
    property indiceConfianca: Integer read FindiceConfianca write FindiceConfianca;
    property pesoBrutoTotal: Integer  read FpesoBrutoTotal  write FpesoBrutoTotal;
    property nroEixos: Integer        read FnroEixos        write FnroEixos;
  end;

implementation

uses
  IniFiles,
  pcnAuxiliar, pcnRetRecepcaoLeitura,
  ACBrUtil, ACBrDFeUtil;

{ TRecepcaoLeitura }

constructor TRecepcaoLeitura.Create;
begin
  inherited Create;

  FGerador := TGerador.Create;
end;

destructor TRecepcaoLeitura.Destroy;
begin
  FGerador.Free;

  inherited;
end;

function TRecepcaoLeitura.GerarXML: Boolean;
var
 sEQP: string;
begin
  Gerador.ArquivoFormatoXML := '';

  sEQP := IntToStrZero(StrToInt64Def(FcEQP, 0), 15);

  Gerador.wGrupo('oneRecepLeitura ' + NAME_SPACE_ONE + ' versao="' + Versao + '"');

  Gerador.wCampo(tcStr, 'BP03', 'tpAmb    ', 01, 01, 1, TpAmbToStr(FtpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, 'BP04', 'verAplic ', 01, 20, 1, FverAplic, DSC_verAplic);
  Gerador.wCampo(tcStr, 'BP05', 'tpTransm ', 01, 01, 1, tpTransmToStr(FtpTransm), DSC_tpTransm);
  Gerador.wCampo(tcStr, 'BP06', 'dhTransm ', 01, 50, 1, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhTransm) +
                                                           GetUTC(CodigoParaUF(FcUF), FdhTransm));

  Gerador.wGrupo('infLeitura');
  Gerador.wCampo(tcInt, 'BP08', 'cUF            ', 01, 02, 1, FcUF, DSC_cUF);
  Gerador.wCampo(tcStr, 'BP09', 'dhPass         ', 01, 50, 1, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhPass) +
                                                              GetUTC(CodigoParaUF(FcUF), FdhPass));
  Gerador.wCampo(tcStr, 'BP10', 'CNPJOper       ', 01, 14, 1, FCNPJOper, DSC_CNPJOper);

  if sEQP <> '' then
    Gerador.wCampo(tcStr, 'BP11', 'cEQP     ', 01, 15, 1, sEQP, DSC_cEQP)
  else
  begin
    Gerador.wCampo(tcDe6, 'BP12', 'latitude ', 01, 10, 1, Flatitude, DSC_Latitude);
    Gerador.wCampo(tcDe6, 'BP13', 'longitude', 01, 10, 1, Flongitude, DSC_Longitude);
    Gerador.wCampo(tcStr, 'BP14', 'tpSentido', 01, 01, 1, TpSentidoToStr(FtpSentido), DSC_tpSentido);
  end;

  Gerador.wCampo(tcStr, 'BP15', 'placa          ', 07, 07, 1, Fplaca, DSC_Placa);
  Gerador.wCampo(tcStr, 'BP16', 'tpVeiculo      ', 01, 01, 0, tpVeiculoToStr(FtpVeiculo), DSC_tpVeiculo);
  Gerador.wCampo(tcInt, 'BP17', 'velocidade     ', 01, 03, 0, Fvelocidade, DSC_Velocidade);
  Gerador.wCampo(tcStr, 'BP18', 'foto           ', 01, 50, 0, Ffoto, DSC_foto);
  Gerador.wCampo(tcInt, 'BP19', 'indiceConfianca', 01, 03, 0, FindiceConfianca, DSC_IndicadorConfianca);
  Gerador.wCampo(tcInt, 'BP20', 'pesoBrutoTotal ', 01, 11, 0, FpesoBrutoTotal, DSC_PesoBrutoTotal);
  Gerador.wCampo(tcInt, 'BP20', 'nroEixos       ', 01, 01, 0, FnroEixos, DSC_NroEixos);

  Gerador.wGrupo('/infLeitura');

  Gerador.wGrupo('/oneRecepLeitura');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

function TRecepcaoLeitura.LerFromIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  sSecao: String;
  ok: Boolean;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    sSecao := 'RECEPCAOLEITURA';
    if INIRec.SectionExists(sSecao) then
    begin
      verAplic        := INIRec.ReadString(sSecao, 'verAplic', '');
      tpTransm        := StrTotpTransm(ok, INIRec.ReadString(sSecao, 'tpTransm', '1'));
      dhTransm        := StringToDateTime(INIRec.ReadString(sSecao, 'dhTransm', ''));
      cUF             := INIRec.ReadInteger(sSecao, 'cUF', 0);
      dhPass          := StringToDateTime(INIRec.ReadString(sSecao, 'dhPass', ''));
      CNPJOper        := INIRec.ReadString(sSecao, 'CNPJOper', '');
      cEQP            := INIRec.ReadString(sSecao, 'cEQP', '');
      latitude        := INIRec.ReadFloat(sSecao, 'latitude', 0);
      longitude       := INIRec.ReadFloat(sSecao, 'longitude', 0);
      tpSentido       := StrTotpSentido(ok,INIRec.ReadString(sSecao, 'tpSentido', 'E'));
      placa           := INIRec.ReadString(sSecao, 'placa', '');
      tpVeiculo       := StrTotpVeiculo(ok,INIRec.ReadString(sSecao, 'tpVeiculo', 'E'));
      velocidade      := INIRec.ReadInteger(sSecao, 'velocidade', 0);
      foto            := INIRec.ReadString(sSecao, 'foto', '');
      indiceConfianca := INIRec.ReadInteger(sSecao, 'indiceConfianca', 0);
      pesoBrutoTotal  := INIRec.ReadInteger(sSecao, 'pesoBrutoTotal', 0);
      nroEixos        := INIRec.ReadInteger(sSecao, 'nroEixos', 0);
    end;
  finally
    INIRec.Free;
  end;
end;

function TRecepcaoLeitura.LerXML(const CaminhoArquivo: String): Boolean;
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

function TRecepcaoLeitura.LerXMLFromString(const AXML: String): Boolean;
var
  RetRecepcaoLeitura : TRetRecepcaoLeitura;
begin
  RetRecepcaoLeitura := TRetRecepcaoLeitura.Create;

  try
    RetRecepcaoLeitura.Leitor.Arquivo := AXML;
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

function TRecepcaoLeitura.ObterNomeArquivo: String;
begin
  Result := FormatDateTime('yyyymmddhhnnss', dhTransm) + '-lei.xml';
end;

end.
