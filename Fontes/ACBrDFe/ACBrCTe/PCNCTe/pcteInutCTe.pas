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

unit pcteInutCTe;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  ACBrDFeConsts,
  pcnAuxiliar, pcnConversao, pcnGerador, pcteRetInutCTe, pcnLeitor,
  pcnSignature, pcteConsts;

type

  { TinutCTe }

  TinutCTe = class(TObject)
  private
    FGerador: TGerador;
    FLeitor: TLeitor;
    FtpAmb: TpcnTipoAmbiente;
    FcUF: Integer;
    Fano: Integer;
    FCNPJ: String;
    Fmodelo: Integer;
    Fserie: Integer;
    FnCTIni: Integer;
    FnCTFin: Integer;
    FxJust: String;
    FIDInutilizacao: String;
    FRetInutCTe: TRetInutCTe;
    FVersao: String;
    Fsignature: Tsignature;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: boolean;
    function LerXML(const CaminhoArquivo: String): boolean;
    function LerXMLFromString(const AXML: String): boolean;
    property Leitor: TLeitor         read FLeitor     write FLeitor;
    property Gerador: TGerador       read FGerador    write FGerador;
    property tpAmb: TpcnTipoAmbiente read FtpAmb      write FtpAmb;
    property cUF: Integer            read FcUF        write FcUF;
    property ano: Integer            read Fano        write Fano;
    property CNPJ: String            read FCNPJ       write FCNPJ;
    property modelo: Integer         read Fmodelo     write Fmodelo;
    property serie: Integer          read Fserie      write Fserie;
    property nCTIni: Integer         read FnCTIni     write FnCTIni;
    property nCTFin: Integer         read FnCTFin     write FnCTFin;
    property xJust: String           read FxJust      write FxJust;
    property ID: String              read FIDInutilizacao write FIDInutilizacao;
    property RetInutCTe: TRetInutCTe read FRetInutCTe write FRetInutCTe;
    property Versao: String          read FVersao     write FVersao;
    property signature: Tsignature   read Fsignature  write Fsignature;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings;

{ TinutCTe }

constructor TinutCTe.Create;
begin
  inherited Create;
  FGerador    := TGerador.Create;
  FRetInutCTe := TRetInutCTe.Create;
  FLeitor     := TLeitor.Create;
  Fsignature  := Tsignature.Create;
end;

destructor TinutCTe.Destroy;
begin
  FGerador.Free;
  FRetInutCTe.Free;
  FLeitor.Free;
  Fsignature.Free;
  inherited;
end;

function TinutCTe.GerarXML: boolean;
begin
  FIDInutilizacao := 'ID' + IntToStrZero(FcUF, 2) +
    OnlyNumber(FCNPJ) + IntToStrZero(Fmodelo, 2) + IntToStrZero(Fserie, 3) +
    IntToStrZero(FnCTIni, 9) + IntToStrZero(FnCTFin, 9);

  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('inutCTe ' + NAME_SPACE_CTE + ' versao="' + Versao + '"');
  Gerador.wGrupo('infInut Id="' + FIDInutilizacao + '"');
  if length(FIDInutilizacao) < 39 then
    Gerador.wAlerta('DP04', 'ID', '', 'ID de inutilização inválido');
  Gerador.wCampo(tcStr, 'DP05', 'tpAmb ', 001, 001, 1, tpAmbToStr(FtpAmb), DSC_TPAMB);
  Gerador.wCampo(tcStr, 'DP06', 'xServ ', 010, 010, 1, 'INUTILIZAR');
  Gerador.wCampo(tcInt, 'DP07', 'cUF   ', 002, 002, 1, FcUF, DSC_CUF);
  if not ValidarCodigoUF(FcUF) then
    Gerador.wAlerta('DP07', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);
  if Fano > 2000 then
    Fano := Fano - 2000;
  Gerador.wCampo(tcInt, 'DP08', 'ano   ', 002, 002, 1, Fano, DSC_ANO);
  Gerador.wCampo(tcStr, 'DP09', 'CNPJ  ', 014, 014, 1, OnlyNumber(FCNPJ), DSC_CNPJ);
  if not ValidarCNPJ(FCNPJ) then
    Gerador.wAlerta('DP09', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
  Gerador.wCampo(tcInt, 'DP10', 'mod   ', 002, 002, 1, Fmodelo, DSC_MOD);
  Gerador.wCampo(tcInt, 'DP11', 'serie ', 001, 003, 1, Fserie, DSC_SERIE);
  Gerador.wCampo(tcInt, 'DP12', 'nCTIni', 001, 009, 1, FnCTIni, DSC_NNFINI);
  Gerador.wCampo(tcInt, 'DP13', 'nCTFin', 001, 009, 1, FnCTFin, DSC_NNFFIN);
  if FnCTIni > FnCTFin then
    Gerador.wAlerta('DP13', 'nCTFin', DSC_NNFFIN, ERR_MSG_FINAL_MENOR_INICIAL);
  Gerador.wCampo(tcStr, 'CP14', 'xJust ', 015, 255, 1, FiltrarTextoXML(true, FxJust), DSC_XJUST);
  Gerador.wGrupo('/infInut');

  if signature.URI <> '' then
  begin
    signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
    signature.GerarXML;
    Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + signature.Gerador.ArquivoFormatoXML;
  end;

  Gerador.wGrupo('/inutCTe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

function TinutCTe.LerXML(const CaminhoArquivo: String): boolean;
var
  ArqInut: TStringList;
begin
  ArqInut := TStringList.Create;
  try
     ArqInut.LoadFromFile(CaminhoArquivo);
     Result := LerXMLFromString(ArqInut.Text);
  finally
     ArqInut.Free;
  end;
end;

function TinutCTe.LerXMLFromString(const AXML: String): boolean;
var
  RetornoInutCTe: TRetInutCTe;
  Ok : Boolean;
begin
  RetornoInutCTe := TRetInutCTe.Create;
  try
    // Lendo dados do pedido de inutilização, se houver...
    Leitor.Arquivo := AXML;
    try
      if ( leitor.rExtrai(1, 'infInut') <> '') then
      begin
        FIDInutilizacao := Leitor.rAtributo('Id=');

        Fversao := Leitor.rAtributo('versao');
        FtpAmb  := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
        FcUF    := Leitor.rCampo(tcInt, 'cUF');
        Fano    := Leitor.rCampo(tcInt, 'ano');
        FCNPJ   := Leitor.rCampo(tcStr, 'CNPJ');
        FModelo := Leitor.rCampo(tcInt, 'mod');
        FSerie  := Leitor.rCampo(tcInt, 'serie');
        FnCTIni := Leitor.rCampo(tcInt, 'nCTIni');
        FnCTFin := Leitor.rCampo(tcInt, 'nCTFin');
        FxJust  := Leitor.rCampo(tcStr, 'xJust');;
      end;

      if Leitor.rExtrai(1, 'Signature') <> '' then
      begin
        signature.URI             := Leitor.rAtributo('Reference URI=');
        signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
        signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
        signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
      end;

      // Lendo dados do retorno, se houver
      RetornoInutCTe.Leitor.Arquivo := AXML;
      Result := RetornoInutCTe.LerXml;

      if ( FIDInutilizacao = '' ) then
      begin
        FIDInutilizacao := RetornoInutCTe.Id;
        tpAmb           := RetornoInutCTe.tpAmb;
      end;

      with FRetInutCTe do
      begin
        ID       := RetornoInutCTe.Id;
        tpAmb    := RetornoInutCTe.tpAmb;
        verAplic := RetornoInutCTe.verAplic;
        cStat    := RetornoInutCTe.cStat;
        xMotivo  := RetornoInutCTe.xMotivo;
        cUF      := RetornoInutCTe.cUF;
        xJust    := RetornoInutCTe.xJust;

        ano      := RetornoInutCTe.ano;
        CNPJ     := RetornoInutCTe.CNPJ;
        Modelo   := RetornoInutCTe.Modelo;
        Serie    := RetornoInutCTe.Serie;
        nCTIni   := RetornoInutCTe.nCTIni;
        nCTFin   := RetornoInutCTe.nCTFin;
        dhRecbto := RetornoInutCTe.dhRecbto;
        nProt    := RetornoInutCTe.nProt;
      end;
    except
      Result := False;
    end;
  finally
    RetornoInutCTe.Free;
  end;
end;

end.
