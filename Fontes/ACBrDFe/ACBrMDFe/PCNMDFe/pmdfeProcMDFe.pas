{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pmdfeProcMDFe;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, pcnLeitor, pmdfeConsts;

type

  TProcMDFe = class(TObject)
  private
    FGerador: TGerador;
    FPathMDFe: String;
    FPathRetConsReciMDFe: String;
    FPathRetConsSitMDFe: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FchMDFe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FVersao: String;
    FcMsg: Integer;
    FxMsg: String;
    FXML_MDFe: String;
    FXML_Prot: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: boolean;
    property Gerador: TGerador           read FGerador             write FGerador;
    property PathMDFe: String            read FPathMDFe            write FPathMDFe;
    property PathRetConsReciMDFe: String read FPathRetConsReciMDFe write FPathRetConsReciMDFe;
    property PathRetConsSitMDFe: String  read FPathRetConsSitMDFe  write FPathRetConsSitMDFe;
    property tpAmb: TpcnTipoAmbiente     read FtpAmb               write FtpAmb;
    property verAplic: String            read FverAplic            write FverAplic;
    property chMDFe: String              read FchMDFe              write FchMDFe;
    property dhRecbto: TDateTime         read FdhRecbto            write FdhRecbto;
    property nProt: String               read FnProt               write FnProt;
    property digVal: String              read FdigVal              write FdigVal;
    property cStat: Integer              read FcStat               write FcStat;
    property xMotivo: String             read FxMotivo             write FxMotivo;
    property Versao: String              read FVersao              write FVersao;
    property cMsg: Integer               read FcMsg                write FcMsg;
    property xMsg: String                read FxMsg                write FxMsg;
    // Usando na Montagem do mdfeProc
    property XML_MDFe: String            read FXML_MDFe            write FXML_MDFe;
    property XML_Prot: String            read FXML_Prot            write FXML_Prot;
  end;

implementation

uses
  StrUtils,
  ACBrDFeConsts,
  ACBrUtil.Strings;

{ TProcMDFe }

constructor TProcMDFe.Create;
begin
  inherited Create;
  FGerador := TGerador.Create;
end;

destructor TProcMDFe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TProcMDFe.GerarXML: boolean;

function PreencherTAG(const TAG: String; Texto: String): String;
begin
  result := '<' + TAG + '>' + RetornarConteudoEntre(Texto, '<' + TAG + '>', '</' + TAG + '>') + '</' + TAG + '>';
end;

var
  XMLMDFe: TStringList;
  XMLinfProt: TStringList;
  XMLinfProt2: TStringList;
  wCstat: String;
  xProtMDFe: String;
  LocLeitor: TLeitor;
  i: Integer;
  ProtLido: Boolean; // Protocolo lido do Arquivo
begin
  XMLMDFe := TStringList.Create;
  XMLinfProt := TStringList.Create;
  XMLinfProt2 := TStringList.Create;
  Gerador.ListaDeAlertas.Clear;

  try
    if (FXML_MDFe = '') and (FXML_Prot = '') then
    begin
      ProtLido := False;
      xProtMDFe := '';
      FnProt := '';

      // Arquivo MDFe
      if not FileExists(FPathMDFe)
       then Gerador.wAlerta('XR04', 'MDFe', 'MDFe', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
       else XMLMDFe.LoadFromFile(FPathMDFe);

      FchMDFe := RetornarConteudoEntre(XMLMDFe.Text, 'Id="MDFe', '"');
      if trim(FchMDFe) = ''
       then Gerador.wAlerta('XR01', 'ID/MDFe', 'Numero da chave do MDFe', ERR_MSG_VAZIO);

      if (FPathRetConsReciMDFe = '') and (FPathRetConsSitMDFe = '')
       then begin
        if (FchMDFe = '') and (FnProt = '')
         then Gerador.wAlerta('XR06', 'RECIBO/SITUAÇÃO', 'RECIBO/SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
         else ProtLido := True;
       end;

      // Gerar arquivo pelo Recibo do MDFe
      if (FPathRetConsReciMDFe <> '') and (FPathRetConsSitMDFe = '') and (not ProtLido)
       then begin
        if not FileExists(FPathRetConsReciMDFe)
         then Gerador.wAlerta('XR06', 'PROTOCOLO', 'PROTOCOLO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
         else begin
          I := 0;
          LocLeitor := TLeitor.Create;
          try
            LocLeitor.CarregarArquivo(FPathRetConsReciMDFe);
            while LocLeitor.rExtrai(1, 'protMDFe', '', i + 1) <> '' do
             begin
               if LocLeitor.rCampo(tcStr, 'chMDFe') = FchMDFe
                then FnProt := LocLeitor.rCampo(tcStr, 'nProt');
               if trim(FnProt) = ''
                then Gerador.wAlerta('XR01', 'PROTOCOLO/MDFe', 'Numero do protocolo', ERR_MSG_VAZIO)
                else begin
                 xProtMDFe := LocLeitor.rExtrai(1, 'protMDFe', '', i + 1);
                 Gerador.ListaDeAlertas.Clear;
                 break;
                end;
                inc(I);
             end;
           finally
             LocLeitor.Free;
           end;
         end;
       end;

      // Gerar arquivo pelo arquivo de consulta da situação do MDFe
      if (FPathRetConsReciMDFe = '') and (FPathRetConsSitMDFe <> '') and (not ProtLido)
       then begin
        if not FileExists(FPathRetConsSitMDFe)
         then Gerador.wAlerta('XR06', 'SITUAÇÃO', 'SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
         else begin
          XMLinfProt.LoadFromFile(FPathRetConsSitMDFe);

          wCstat:=RetornarConteudoEntre(XMLinfProt.text, '<cStat>', '</cStat>');
          if trim(wCstat) = '101'
           then XMLinfProt2.Text:=RetornarConteudoEntre(XMLinfProt.text, '<infCanc', '</infCanc>')
           else XMLinfProt2.Text:=RetornarConteudoEntre(XMLinfProt.text, '<infProt', '</infProt>');

          xProtMDFe := '<protMDFe versao="' + Versao + '">' +
                        '<infProt>' +
                          PreencherTAG('tpAmb', XMLinfProt.text) +
                          PreencherTAG('verAplic', XMLinfProt.text) +
                          PreencherTAG('chMDFe', XMLinfProt.text) +
                          PreencherTAG('dhRecbto', XMLinfProt2.text) +
                          PreencherTAG('nProt', XMLinfProt2.text) +
                          PreencherTAG('digVal', XMLinfProt.text) +
                          PreencherTAG('cStat', XMLinfProt.text) +
                          PreencherTAG('xMotivo', XMLinfProt.text) +
                        '</infProt>' +
                        IfThen( (PreencherTAG('cMsg', XMLinfProt.text) <> ''),
                        '<infFisco>' +
                          PreencherTAG('cMsg', XMLinfProt.text) +
                          PreencherTAG('xMsg', XMLinfProt.text) +
                        '</infFisco>',
                        '') +
                       '</protMDFe>';
         end;
       end;

      if ProtLido
       then begin
        xProtMDFe := '<protMDFe versao="' + Versao + '">' +
                      '<infProt>' +
                       '<tpAmb>'+TpAmbToStr(FtpAmb)+'</tpAmb>'+
                       '<verAplic>'+FverAplic+'</verAplic>'+
                       '<chMDFe>'+FchMDFe+'</chMDFe>'+
                       '<dhRecbto>'+FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',FdhRecbto)+'</dhRecbto>'+
                       '<nProt>'+FnProt+'</nProt>'+
                       '<digVal>'+FdigVal+'</digVal>'+
                       '<cStat>'+IntToStr(FcStat)+'</cStat>'+
                       '<xMotivo>'+FxMotivo+'</xMotivo>'+
                      '</infProt>'+
                      IfThen( (cMsg > 0) or (xMsg <> ''),
                      '<infFisco>' +
                        '<cMsg>' + IntToStr(FcMsg) + '</cMsg>' +
                        '<xMsg>' + FxMsg + '</xMsg>' +
                      '</infFisco>',
                      '') +
                     '</protMDFe>';
       end;

      FXML_MDFe := XMLMDFe.Text;
      FXML_Prot := xProtMDFe;
    end;

    // Gerar arquivo
    if (Gerador.ListaDeAlertas.Count = 0) and
       (FXML_MDFe <> '') and (FXML_Prot <> '') then
    begin
      Gerador.ArquivoFormatoXML := '';
      Gerador.wGrupo(ENCODING_UTF8, '', False);
      Gerador.wGrupo('mdfeProc versao="' + Versao + '" ' + NAME_SPACE_MDFE, '');
      Gerador.wTexto('<MDFe xmlns' + RetornarConteudoEntre(FXML_MDFe, '<MDFe xmlns', '</MDFe>') + '</MDFe>');
      Gerador.wTexto(FXML_Prot);
      Gerador.wGrupo('/mdfeProc');
    end;

    Result := (Gerador.ListaDeAlertas.Count = 0);

  finally
    XMLMDFe.Free;
    XMLinfProt.Free;
    XMLinfProt2.Free;
  end;
end;

end.

