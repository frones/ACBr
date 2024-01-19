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

unit ACBrNFComProc;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase;

type

  TProcNFCom = class(TObject)
  private
    FPathNFCom: string;
    FPathRetConsReciNFCom: string;
    FPathRetConsSitNFCom: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FchNFCom: string;
    FdhRecbto: TDateTime;
    FnProt: string;
    FdigVal: string;
    FcStat: Integer;
    FxMotivo: string;
    FVersao: string;
    FcMsg: Integer;
    FxMsg: string;
    FId: string;

    // Usando na Montagem do NFComProc
    FXML_NFCom: string;
    FXML_prot: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TProcNFCom);
    function GerarXML: string;

    property PathNFCom: string read FPathNFCom write FPathNFCom;
    property PathRetConsReciNFCom: string read FPathRetConsReciNFCom write FPathRetConsReciNFCom;
    property PathRetConsSitNFCom: string read FPathRetConsSitNFCom write FPathRetConsSitNFCom;
    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property verAplic: string read FverAplic write FverAplic;
    property chNFCom: string read FchNFCom write FchNFCom;
    property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property nProt: string read FnProt write FnProt;
    property digVal: string read FdigVal write FdigVal;
    property cStat: Integer read FcStat write FcStat;
    property xMotivo: string read FxMotivo write FxMotivo;
    property Versao: string read FVersao write FVersao;
    property cMsg: Integer read FcMsg write FcMsg;
    property xMsg: string read FxMsg write FxMsg;
    property Id: string read FId write FId;

    // Usando na Montagem do NFComProc
    property XML_NFCom: string read FXML_NFCom write FXML_NFCom;
    property XML_prot: string read FXML_prot write FXML_prot;
  end;

implementation

uses
//  ACBrDFeConsts,
  ACBrUtil.Strings,
  ACBrXmlDocument,
  pcnAuxiliar,
  ACBrNFComConsts;

{ TProcNFCom }

constructor TProcNFCom.Create;
begin
  inherited Create;

  FnProt   := '';
end;

destructor TProcNFCom.Destroy;
begin

  inherited;
end;

function TProcNFCom.GerarXML: string;

  function PreencherTAG(const TAG: string; Texto: string): string;
  begin
    result := '<' + TAG + '>' + RetornarConteudoEntre(Texto, '<' + TAG + '>', '</' + TAG + '>') + '</' + TAG + '>';
  end;

var
  XMLNFCom: TStringList;
  XMLinfProt: TStringList;
  XMLinfProt2: TStringList;
  wCstat: string;
  xProtNFCom: string;
  nProtLoc: string;
  xUF: string;
  ProtLido: Boolean; //Protocolo lido do arquivo
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
begin
  XMLNFCom := TStringList.Create;
  XMLinfProt := TStringList.Create;
  XMLinfProt2 := TStringList.Create;

  try
    if (FXML_NFCom = '') and (FXML_prot = '') then
    begin
      ProtLido := False;
      xProtNFCom := '';

      // Arquivo NFCom
      if not FileExists(FPathNFCom) then
        raise Exception.Create(ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
      else
        XMLNFCom.LoadFromFile(FPathNFCom);

      FchNFCom := RetornarConteudoEntre(XMLNFCom.Text, 'Id="NFCom', '"');

      if trim(FchNFCom) = '' then
        raise Exception.Create('Numero da chave da NFCom:' + ERR_MSG_VAZIO);

      if (FPathRetConsReciNFCom = '') and (FPathRetConsSitNFCom = '') then
      begin
        if (FchNFCom = '') and (FnProt = '') then
          raise Exception.Create(ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
          ProtLido := True;
      end;

      // Gerar arquivo pelo Recibo da NFCom
      if (FPathRetConsReciNFCom <> '') and (FPathRetConsSitNFCom = '') and
         (not ProtLido) then
      begin
        if not FileExists(FPathRetConsReciNFCom) then
          raise Exception.Create(ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          Document := TACBrXmlDocument.Create;

          try
            // carregar o arquivo do disco
            XMLinfProt.LoadFromFile(FPathRetConsReciNFCom);
            FXML_prot := XMLinfProt.Text;
            Document.LoadFromXml(FXML_prot);

            ANode := Document.Root;

            if ANode <> nil then
            begin
              ANode := ANode.Childrens.FindAnyNs('infProt');

              if ANode <> nil then
                nProtLoc := ObterConteudoTag(ANode.Childrens.Find('nProt'), tcStr);
            end;

            FreeAndNil(Document);
            Result := nProtLoc;
          except
            Result := '';
          end;
        end;
      end;

      // Gerar arquivo pelo arquivo de consulta da situação da NFCom
      if (FPathRetConsReciNFCom = '') and (FPathRetConsSitNFCom <> '') and (not ProtLido) then
      begin
        if not FileExists(FPathRetConsSitNFCom) then
          raise Exception.Create(ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          XMLinfProt.LoadFromFile(FPathRetConsSitNFCom);

          wCstat := RetornarConteudoEntre(XMLinfProt.text, '<cStat>', '</cStat>');

          if ((trim(wCstat) = '101') or
              (trim(wCstat) = '151') or
              (trim(wCstat) = '155')) then //esta cancelada
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infCanc', '</infCanc>')
          else
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infProt', '</infProt>');

          nProtLoc := RetornarConteudoEntre(XMLinfProt2.text, '<nProt>', '</nProt>');

          xProtNFCom := '<protNFCom versao="' + Versao + '">' +
                         '<infProt Id="ID'+ nProtLoc +'">'+
                          PreencherTAG('tpAmb', XMLinfProt.text) +
                          PreencherTAG('verAplic', XMLinfProt.text) +
                          PreencherTAG('chNFCom', XMLinfProt.text) +
                          PreencherTAG('dhRecbto', XMLinfProt2.text) +
                          PreencherTAG('nProt', XMLinfProt2.text) +
                          PreencherTAG('digVal', XMLinfProt.text) +
                          PreencherTAG('cStat', XMLinfProt.text) +
                          PreencherTAG('xMotivo', XMLinfProt.text) +
                          PreencherTAG('cMsg', XMLinfProt.text) +
                          PreencherTAG('xMsg', XMLinfProt.text) +
                         '</infProt>' +
                        '</protNFCom>';
        end;
      end;

      if ProtLido then
      begin
        if Copy(FverAplic,1,2) = 'SV' then
          xUF := CodigoParaUF(StrToIntDef(Copy(FchNFCom, 1, 2), 0))
        else
          xUF := Copy(FverAplic,1,2);

        xProtNFCom := '<protNFCom versao="' + Versao + '">' +
                       '<infProt Id="' + IIf( Pos('ID', FnProt) > 0, FnProt, 'ID' + FnProt ) + '">' +
                         '<tpAmb>' + TipoAmbienteToStr(FtpAmb) + '</tpAmb>' +
                         '<verAplic>' + FverAplic + '</verAplic>' +
                         '<chNFCom>' + FchNFCom + '</chNFCom>' +
                         '<dhRecbto>' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhRecbto) +
                                        IIf(Versao >= '3.10', GetUTC(xUF,FdhRecbto),'') +
                         '</dhRecbto>'+
                         '<nProt>' + FnProt + '</nProt>' +
                         '<digVal>' + FdigVal + '</digVal>' +
                         '<cStat>' + IntToStr(FcStat) + '</cStat>' +
                         '<xMotivo>' + FxMotivo + '</xMotivo>' +
                         '<cMsg>' + IntToStr(FcMsg) + '</cMsg>' +
                         '<xMsg>' + FxMsg + '</xMsg>' +
                       '</infProt>' +
                      '</protNFCom>';
      end;

      FXML_NFCom := XMLNFCom.Text;
      FXML_prot := xProtNFCom;
    end;

    // Gerar arquivo
    if (FXML_NFCom <> '') and (FXML_prot <> '') then
      Result := '<nfComProc ' + NAME_SPACE_NFCom + ' versao="' + versao + '">' +
                  '<NFCom xmlns>' + RetornarConteudoEntre(FXML_NFCom, '<NFCom xmlns', '</NFCom>') +
                  '</NFCom>' +
                   FXML_prot +
                '</nfComProc>'
    else
      Result := '';
  finally
    XMLNFCom.Free;
    XMLinfProt.Free;
    XMLinfProt2.Free;
  end;
end;

procedure TProcNFCom.Assign(Source: TProcNFCom);
begin
  PathNFCom := Source.PathNFCom;
  PathRetConsReciNFCom := Source.PathRetConsReciNFCom;
  PathRetConsSitNFCom := Source.PathRetConsSitNFCom;
  tpAmb := Source.tpAmb;
  verAplic := Source.verAplic;
  chNFCom := Source.chNFCom;
  dhRecbto := Source.dhRecbto;
  nProt := Source.nProt;
  digVal := Source.digVal;
  cStat := Source.cStat;
  xMotivo := Source.xMotivo;
  Versao := Source.Versao;
  cMsg := Source.cMsg;
  xMsg := Source.xMsg;

  XML_NFCom := Source.XML_NFCom;
  XML_prot := Source.XML_prot;
end;

end.

