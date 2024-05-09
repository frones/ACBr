{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrNFe.Inut;

interface

uses
  SysUtils, Classes,
  pcnNFeConsts,
  pcnConversao,
  ACBrNFe.RetInut,
  pcnSignature,
  ACBrXmlBase;

type

  { TinutNFe }

  TinutNFe = class(TObject)
  private
    FtpAmb: TpcnTipoAmbiente;
    FcUF: Integer;
    Fano: Integer;
    FCNPJ: String;
    Fmodelo: Integer;
    Fserie: Integer;
    FnNFIni: Integer;
    FnNFFin: Integer;
    FxJust: String;
    FIDInutilizacao: String;
    FVersao: String;
    FRetInutNFe: TRetInutNFe;
    Fsignature: Tsignature;
    FXML: String;
  public
    constructor Create;
    destructor Destroy; override;

    function GerarXML: string;

    function LerXML(const CaminhoArquivo: String): Boolean;
    function LerXMLFromString(const AXML: String): Boolean;
    function ObterNomeArquivo: String;

    property tpAmb: TpcnTipoAmbiente read FtpAmb      write FtpAmb;
    property cUF: Integer            read FcUF        write FcUF;
    property ano: Integer            read Fano        write Fano;
    property CNPJ: String            read FCNPJ       write FCNPJ;
    property modelo: Integer         read Fmodelo     write Fmodelo;
    property serie: Integer          read Fserie      write Fserie;
    property nNFIni: Integer         read FnNFIni     write FnNFIni;
    property nNFFin: Integer         read FnNFFin     write FnNFFin;
    property xJust: String           read FxJust      write FxJust;
    property ID: String              read FIDInutilizacao write FIDInutilizacao;
    property Versao: String          read FVersao     write FVersao;
    property RetInutNFe: TRetInutNFe read FRetInutNFe write FRetInutNFe;
    property signature: Tsignature   read Fsignature  write Fsignature;
    property XML: String             read FXML        write FXML;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrXmlDocument;

{ TinutNFe }

constructor TinutNFe.Create;
begin
  inherited Create;

  FRetInutNFe := TRetInutNFe.Create;
  Fsignature  := Tsignature.Create;
end;

destructor TinutNFe.Destroy;
begin
  FRetInutNFe.Free;
  Fsignature.Free;

  inherited;
end;

function TinutNFe.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(FIDInutilizacao) + '-ped-inu.xml';
end;

function TinutNFe.GerarXML: string;
var
  xCNPJ, ACNPJ, AJust: String;
begin
  ACNPJ := OnlyNumber(FCNPJ);

  if (cUF in [51]) and (Length(ACNPJ) = 11) then
    ACNPJ := '000' + ACNPJ;

  FIDInutilizacao := 'ID' + IntToStrZero(cUF, 2) +  Copy(IntToStrZero(ano, 4), 3, 2) +
                     ACNPJ + IntToStrZero(modelo, 2) + IntToStrZero(serie, 3) +
                     IntToStrZero(nNFIni, 9) + IntToStrZero(nNFFin, 9);

//  if length(FIDInutilizacao) < 43 then
//    Gerador.wAlerta('DP04', 'ID', '', 'ID de inutilização inválido');

//  if not ValidarCodigoUF(FcUF) then
//    Gerador.wAlerta('DP07', 'cUF', DSC_CUF, ERR_MSG_INVALIDO);

  if ano > 2000 then
    ano := ano - 2000;

  ACNPJ := OnlyNumber(FCNPJ);

  if cUF in [51] then
  begin
    if Length(ACNPJ) = 11 then
      xCNPJ := '<CPF>' + ACNPJ + '</CPF>'
    else
      xCNPJ := '<CNPJ>' + ACNPJ + '</CNPJ>';
  end
  else
  begin
    xCNPJ := '<CNPJ>' + ACNPJ + '</CNPJ>';

//    if not ValidarCNPJ(FCNPJ) then
//      Gerador.wAlerta('DP09', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
  end;

//  if nNFIni > nNFFin then
//    Gerador.wAlerta('DP13', 'nNFFin', DSC_NNFFIN, ERR_MSG_FINAL_MENOR_INICIAL);

  Result := '<inutNFe ' + NAME_SPACE + ' versao="' + Fversao + '">' +
              '<infInut Id="' + FIDInutilizacao + '">' +
                '<tpAmb>' + tpAmbToStr(tpAmb) + '</tpAmb>' +
                '<xServ>INUTILIZAR</xServ>' +
                '<cUF>' + IntToStr(cUF) + '</cUF>' +
                '<ano>' + IntToStr(ano) + '</ano>' +
                xCNPJ +
                '<mod>' + IntToStr(modelo) + '</mod>' +
                '<serie>' + IntToStr(serie) + '</serie>' +
                '<nNFIni>' + IntToStr(nNFIni) + '</nNFIni>' +
                '<nNFFin>' + IntToStr(nNFFin) + '</nNFFin>' +
                '<xJust>' + xJust + '</xJust>' +
              '</infInut>' +
            '</inutNFe>';

  if signature.URI <> '' then
  begin
    signature.GerarXML;
    Result := Result + signature.Gerador.ArquivoFormatoXML;
  end;
end;

function TinutNFe.LerXML(const CaminhoArquivo: String): Boolean;
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

function TinutNFe.LerXMLFromString(const AXML: String): Boolean;
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ok: Boolean;
  RetornoInutNFe: TRetInutNFe;
  CPF: string;
begin
  RetornoInutNFe := TRetInutNFe.Create;
  try
    Document := TACBrXmlDocument.Create;

    try
      try
        Document.LoadFromXml(AXML);

        ANode := Document.Root;

        if ANode <> nil then
        begin
          XML := AXML;

          // versao é um atributo da tag inutNFe
          versao := ObterConteudoTag(ANode.Attributes.Items['versao']);

          AuxNode := ANode.Childrens.FindAnyNs('infInut');

          if AuxNode <> nil then
          begin
            FIDInutilizacao := ObterConteudoTag(AuxNode.Attributes.Items['Id']);
            tpAmb := StrToTpAmb(ok, ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpAmb'), tcStr));
            cUF := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cUF'), tcInt);
            ano := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ano'), tcInt);
            CNPJ := ObterConteudoTagCNPJCPF(AuxNode);
            modelo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('mod'), tcInt);
            serie := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('serie'), tcInt);
            nNFIni := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nNFIni'), tcInt);
            nNFFin := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nNFFin'), tcInt);
            xJust := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xJust'), tcStr);
          end;

          LerSignature(ANode.Childrens.Find('Signature'), signature);
        end;

        // Lendo dados do retorno, se houver
        RetornoInutNFe.XmlRetorno := AXML;

        Result := RetornoInutNFe.LerXml;

        if ( FIDInutilizacao = '' ) then
        begin
          FIDInutilizacao := RetornoInutNFe.Id;
          tpAmb           := RetornoInutNFe.tpAmb;
        end;

        with FRetInutNFe do
        begin
          Id       := RetornoInutNFe.Id;
          tpAmb    := RetornoInutNFe.tpAmb;
          verAplic := RetornoInutNFe.verAplic;
          cStat    := RetornoInutNFe.cStat;
          xMotivo  := RetornoInutNFe.xMotivo;
          cUF      := RetornoInutNFe.cUF;
          xJust    := RetornoInutNFe.xJust;

          ano      := RetornoInutNFe.ano;
          CNPJ     := RetornoInutNFe.CNPJ;
          Modelo   := RetornoInutNFe.Modelo;
          Serie    := RetornoInutNFe.Serie;
          nNFIni   := RetornoInutNFe.nNFIni;
          nNFFin   := RetornoInutNFe.nNFFin;
          dhRecbto := RetornoInutNFe.dhRecbto;
          nProt    := RetornoInutNFe.nProt;
        end;

        Result := True;
      except
        Result := False;
      end;
    finally
      FreeAndNil(Document);
    end;
  finally
    RetornoInutNFe.Free;
  end;
end;

end.

