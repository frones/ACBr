{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrNF3eProc;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase;

type

  TProcNF3e = class(TObject)
  private
    FPathNF3e: String;
    FPathRetConsReciNF3e: String;
    FPathRetConsSitNF3e: String;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: String;
    FchNF3e: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FVersao: String;
    FcMsg: Integer;
    FxMsg: String;

    // Usando na Montagem do NF3eProc
    FXML_NF3e: String;
    FXML_prot: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TProcNF3e);
    function GerarXML: string;

    property PathNF3e: String           read FPathNF3e            write FPathNF3e;
    property PathRetConsReciNF3e: String read FPathRetConsReciNF3e write FPathRetConsReciNF3e;
    property PathRetConsSitNF3e: String  read FPathRetConsSitNF3e  write FPathRetConsSitNF3e;
    property tpAmb: TACBrTipoAmbiente   read FtpAmb              write FtpAmb;
    property verAplic: String           read FverAplic           write FverAplic;
    property chNF3e: String             read FchNF3e             write FchNF3e;
    property dhRecbto: TDateTime        read FdhRecbto           write FdhRecbto;
    property nProt: String              read FnProt              write FnProt;
    property digVal: String             read FdigVal             write FdigVal;
    property cStat: Integer             read FcStat              write FcStat;
    property xMotivo: String            read FxMotivo            write FxMotivo;
    property Versao: String             read FVersao             write FVersao;
    property cMsg: Integer              read FcMsg               write FcMsg;
    property xMsg: String               read FxMsg               write FxMsg;

    // Usando na Montagem do NF3eProc
    property XML_NF3e: String           read FXML_NF3e           write FXML_NF3e;
    property XML_prot: String           read FXML_prot           write FXML_prot;
  end;

implementation

uses
  StrUtils,
  ACBrDFeConsts,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrXmlDocument,
  ACBrNF3eConsts;

{ TProcNF3e }

constructor TProcNF3e.Create;
begin
  inherited Create;

  FnProt   := '';
end;

destructor TProcNF3e.Destroy;
begin

  inherited;
end;

function TProcNF3e.GerarXML: string;

  function PreencherTAG(const TAG: String; Texto: String): String;
  begin
    result := '<' + TAG + '>' + RetornarConteudoEntre(Texto, '<' + TAG + '>', '</' + TAG + '>') + '</' + TAG + '>';
  end;

var
  XMLNF3e: TStringList;
  XMLinfProt: TStringList;
  XMLinfProt2: TStringList;
  wCstat: String;
  xProtNF3e: String;
  nProtLoc: String;
  xUF: string;
  i: Integer;
  ProtLido: Boolean; //Protocolo lido do arquivo
  Document: TACBrXmlDocument;
  ANode, AnodeAux: TACBrXmlNode;
begin
  XMLNF3e := TStringList.Create;
  XMLinfProt := TStringList.Create;
  XMLinfProt2 := TStringList.Create;

  try
    if (FXML_NF3e = '') and (FXML_prot = '') then
    begin
      ProtLido := False;
      xProtNF3e := '';

      // Arquivo NF3e
      if not FileExists(FPathNF3e) then
        raise Exception.Create(ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
      else
        XMLNF3e.LoadFromFile(FPathNF3e);

      FchNF3e := RetornarConteudoEntre(XMLNF3e.Text, 'Id="NF3e', '"');

      if trim(FchNF3e) = '' then
        raise Exception.Create('Numero da chave da NF3e:' + ERR_MSG_VAZIO);

      if (FPathRetConsReciNF3e = '') and (FPathRetConsSitNF3e = '') then
      begin
        if (FchNF3e = '') and (FnProt = '') then
          raise Exception.Create(ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
          ProtLido := True;
      end;

      // Gerar arquivo pelo Recibo da NF3e
      if (FPathRetConsReciNF3e <> '') and (FPathRetConsSitNF3e = '') and (not ProtLido) then
      begin
        if not FileExists(FPathRetConsReciNF3e) then
          raise Exception.Create(ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          Document := TACBrXmlDocument.Create;

          try
            XMLinfProt.LoadFromFile(FPathRetConsReciNF3e);
            FXML_prot := XMLinfProt.Text; // carregar o arquivo do disco
            Document.LoadFromXml(FXML_prot);

            ANode := Document.Root; //.Childrens.FindAnyNs('retConsSitNF3e');

            if ANode <> nil then
            begin
              // falta implementar
            end;

            FreeAndNil(Document);
            Result := ''; // retornar o protocolo
          except
            Result := '';
          end;

          {
          i := 0;
          LocLeitor := TLeitor.Create;
          try
            LocLeitor.CarregarArquivo(FPathRetConsReciNF3e);
            while LocLeitor.rExtrai(1, 'protNF3e', '', i + 1) <> '' do
            begin
              if LocLeitor.rCampo(tcStr, 'chNF3e') = FchNF3e then
                FnProt := LocLeitor.rCampo(tcStr, 'nProt');

              if trim(FnProt) = '' then
                Gerador.wAlerta('XR01', 'PROTOCOLO/NF3e', 'Numero do protocolo', ERR_MSG_VAZIO)
              else
              begin
                xProtNF3e := LocLeitor.rExtrai(1, 'protNF3e', '', i + 1); // +'</protNF3e>';
                Gerador.ListaDeAlertas.Clear;
                break;
              end;

              Inc(i);
            end;
          finally
            LocLeitor.Free;
          end;
          }
        end;
      end;

      // Gerar arquivo pelo arquivo de consulta da situação da NF3e
      if (FPathRetConsReciNF3e = '') and (FPathRetConsSitNF3e <> '') and (not ProtLido) then
      begin
        if not FileExists(FPathRetConsSitNF3e) then
          raise Exception.Create(ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          XMLinfProt.LoadFromFile(FPathRetConsSitNF3e);

          wCstat := RetornarConteudoEntre(XMLinfProt.text, '<cStat>', '</cStat>');

          if ((trim(wCstat) = '101') or
              (trim(wCstat) = '151') or
              (trim(wCstat) = '155')) then //esta cancelada
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infCanc', '</infCanc>')
          else
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infProt', '</infProt>');

          nProtLoc := RetornarConteudoEntre(XMLinfProt2.text, '<nProt>', '</nProt>');

          xProtNF3e := '<protNF3e versao="' + Versao + '">' +
                       '<infProt Id="ID'+ nProtLoc +'">'+
                        PreencherTAG('tpAmb', XMLinfProt.text) +
                        PreencherTAG('verAplic', XMLinfProt.text) +
                        PreencherTAG('chNF3e', XMLinfProt.text) +
                        PreencherTAG('dhRecbto', XMLinfProt2.text) +
                        PreencherTAG('nProt', XMLinfProt2.text) +
                        PreencherTAG('digVal', XMLinfProt.text) +
                        PreencherTAG('cStat', XMLinfProt.text) +
                        PreencherTAG('xMotivo', XMLinfProt.text) +
                        PreencherTAG('cMsg', XMLinfProt.text) +
                        PreencherTAG('xMsg', XMLinfProt.text) +
                       '</infProt>' +
                      '</protNF3e>';
        end;
      end;

      if ProtLido then
      begin
        if Copy(FverAplic,1,2) = 'SV' then
          xUF := CodigoUFparaUF(StrToIntDef(Copy(FchNF3e, 1, 2), 0))
        else
          xUF := Copy(FverAplic,1,2);

        xProtNF3e := '<protNF3e versao="' + Versao + '">' +
                       '<infProt Id="' + IfThen( Pos('ID', FnProt) > 0, FnProt, 'ID' + FnProt ) + '">' +
                         '<tpAmb>' + TipoAmbienteToStr(FtpAmb) + '</tpAmb>' +
                         '<verAplic>' + FverAplic + '</verAplic>' +
                         '<chNF3e>' + FchNF3e + '</chNF3e>' +
                         '<dhRecbto>' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhRecbto) +
                                        IfThen(Versao >= '3.10', GetUTC(xUF,FdhRecbto),'') +
                         '</dhRecbto>'+
                         '<nProt>' + FnProt + '</nProt>' +
                         '<digVal>' + FdigVal + '</digVal>' +
                         '<cStat>' + IntToStr(FcStat) + '</cStat>' +
                         '<xMotivo>' + FxMotivo + '</xMotivo>' +
                         '<cMsg>' + IntToStr(FcMsg) + '</cMsg>' +
                         '<xMsg>' + FxMsg + '</xMsg>' +
                       '</infProt>' +
                     '</protNF3e>';
      end;

      FXML_NF3e := XMLNF3e.Text;
      FXML_prot := xProtNF3e;
    end;

    // Gerar arquivo
    if (FXML_NF3e <> '') and (FXML_prot <> '') then
      Result := '<nf3eProc ' + NAME_SPACE_NF3e + ' versao="' + versao + '">' +
                  '<NF3e xmlns>' + RetornarConteudoEntre(FXML_NF3e, '<NF3e xmlns', '</NF3e>') +
                  '</NF3e>' +
                   FXML_prot +
                '</nf3eProc>'
    else
      Result := '';
  finally
    XMLNF3e.Free;
    XMLinfProt.Free;
    XMLinfProt2.Free;
  end;
end;

procedure TProcNF3e.Assign(Source: TProcNF3e);
begin
  PathNF3e            := Source.PathNF3e;
  PathRetConsReciNF3e := Source.PathRetConsReciNF3e;
  PathRetConsSitNF3e  := Source.PathRetConsSitNF3e;
  tpAmb               := Source.tpAmb;
  verAplic            := Source.verAplic;
  chNF3e              := Source.chNF3e;
  dhRecbto            := Source.dhRecbto;
  nProt               := Source.nProt;
  digVal              := Source.digVal;
  cStat               := Source.cStat;
  xMotivo             := Source.xMotivo;
  Versao              := Source.Versao;
  cMsg                := Source.cMsg;
  xMsg                := Source.xMsg;

  XML_NF3e := Source.XML_NF3e;
  XML_prot := Source.XML_prot;
end;

end.

