////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnProcNFe;

interface

uses
  SysUtils, Classes,
  ACBrDFeConsts,
  pcnNFeConsts,
  pcnConversao, pcnGerador;

type

//  TPcnPadraoNomeProcNFe = (tpnPublico, tpnPrivado);

  TProcNFe = class(TObject)
  private
    FGerador: TGerador;
    FPathNFe: String;
    FPathRetConsReciNFe: String;
    FPathRetConsSitNFe: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FchNFe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FVersao: String;
    FcMsg: Integer;
    FxMsg: String;

    // Usando na Montagem do nfeProc
    FXML_NFe: String;
    FXML_prot: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TProcNFe);
    function GerarXML: Boolean;
//    function ObterNomeArquivo(const PadraoNome: TPcnPadraoNomeProcNFe = tpnPrivado): String;

    property Gerador: TGerador          read FGerador;
    property PathNFe: String            read FPathNFe            write FPathNFe;
    property PathRetConsReciNFe: String read FPathRetConsReciNFe write FPathRetConsReciNFe;
    property PathRetConsSitNFe: String  read FPathRetConsSitNFe  write FPathRetConsSitNFe;
    property tpAmb: TpcnTipoAmbiente    read FtpAmb              write FtpAmb;
    property verAplic: String           read FverAplic           write FverAplic;
    property chNFe: String              read FchNFe              write FchNFe;
    property dhRecbto: TDateTime        read FdhRecbto           write FdhRecbto;
    property nProt: String              read FnProt              write FnProt;
    property digVal: String             read FdigVal             write FdigVal;
    property cStat: Integer             read FcStat              write FcStat;
    property xMotivo: String            read FxMotivo            write FxMotivo;
    property Versao: String             read FVersao             write FVersao;
    property cMsg: Integer              read FcMsg               write FcMsg;
    property xMsg: String               read FxMsg               write FxMsg;

    // Usando na Montagem do nfeProc
    property XML_NFe: String            read FXML_NFe            write FXML_NFe;
    property XML_prot: String           read FXML_prot           write FXML_prot;
  end;

implementation

uses
  StrUtils,
  pcnLeitor,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.Strings;

{ TProcNFe }

constructor TProcNFe.Create;
begin
  inherited Create;
  FGerador := TGerador.Create;
  FnProt   := '';
end;

destructor TProcNFe.Destroy;
begin
  FGerador.Free;
  inherited;
end;
(*
function TProcNFe.ObterNomeArquivo(const PadraoNome: TPcnPadraoNomeProcNFe = tpnPrivado): String;
var
  s: String;
begin
  Result := FchNFe + '-procNFe.xml';
  if PadraoNome = tpnPublico then
  begin
    s := '00' + Versao;
    Result := FnProt + '_v' + copy(s, length(s) - 4, 5) + '-procNFe.xml';
  end;
end;
*)
function TProcNFe.GerarXML: Boolean;

  function PreencherTAG(const TAG: String; Texto: String): String;
  begin
    result := '<' + TAG + '>' + RetornarConteudoEntre(Texto, '<' + TAG + '>', '</' + TAG + '>') + '</' + TAG + '>';
  end;

var
  XMLNFe: TStringList;
  XMLinfProt: TStringList;
  XMLinfProt2: TStringList;
  wCstat: String;
  xProtNFe: String;
  nProtLoc: String;
  xUF: string;    (*By Edilson Alves de Oliveira*)
  LocLeitor: TLeitor;
  i: Integer;
  ProtLido: Boolean; //Protocolo lido do arquivo
begin
  XMLNFe      := TStringList.Create;
  XMLinfProt  := TStringList.Create;
  XMLinfProt2 := TStringList.Create;
  Gerador.ListaDeAlertas.Clear;

  try
    if (FXML_NFe = '') and (FXML_prot = '') then
    begin
      ProtLido := False;
      xProtNFe    := '';

      // Arquivo NFe
      if not FileExists(FPathNFe) then
        Gerador.wAlerta('XR04', 'NFE', 'NFE', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
      else
        XMLNFE.LoadFromFile(FPathNFe);

      FchNFe := RetornarConteudoEntre(XMLNFE.Text, 'Id="NFe', '"');

      if trim(FchNFe) = '' then
        Gerador.wAlerta('XR01', 'ID/NFE', 'Numero da chave da NFe', ERR_MSG_VAZIO);

      if (FPathRetConsReciNFe = '') and (FPathRetConsSitNFe = '') then
      begin
        if (FchNFe = '') and (FnProt = '') then
          Gerador.wAlerta('XR06', 'RECIBO/SITUAÇÃO', 'RECIBO/SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
          ProtLido := True;
      end;

      // Gerar arquivo pelo Recibo da NFe                                       //
      if (FPathRetConsReciNFe <> '') and (FPathRetConsSitNFe = '') and (not ProtLido) then
      begin
        if not FileExists(FPathRetConsReciNFe) then
          Gerador.wAlerta('XR06', 'PROTOCOLO', 'PROTOCOLO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          I := 0;
          LocLeitor := TLeitor.Create;
          try
            LocLeitor.CarregarArquivo(FPathRetConsReciNFe);
            while LocLeitor.rExtrai(1, 'protNFe', '', i + 1) <> '' do
            begin
              if LocLeitor.rCampo(tcStr, 'chNFe') = FchNFe then
                FnProt := LocLeitor.rCampo(tcStr, 'nProt');

              if trim(FnProt) = '' then
                Gerador.wAlerta('XR01', 'PROTOCOLO/NFe', 'Numero do protocolo', ERR_MSG_VAZIO)
              else
              begin
                xProtNFe := LocLeitor.rExtrai(1, 'protNFe', '', i + 1); // +'</protNFe>';
                Gerador.ListaDeAlertas.Clear;
                break;
              end;

              I := I + 1;
            end;
          finally
            LocLeitor.Free;
          end;
        end;
      end;

      // Gerar arquivo pelo arquivo de consulta da situação da NFe              //
      if (FPathRetConsReciNFe = '') and (FPathRetConsSitNFe <> '') and (not ProtLido) then
      begin
        if not FileExists(FPathRetConsSitNFe) then
          Gerador.wAlerta('XR06', 'SITUAÇÃO', 'SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          XMLinfProt.LoadFromFile(FPathRetConsSitNFe);

          wCstat := RetornarConteudoEntre(XMLinfProt.text, '<cStat>', '</cStat>');

          if ((trim(wCstat) = '101') or
              (trim(wCstat) = '151') or
              (trim(wCstat) = '155')) then //esta cancelada
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infCanc', '</infCanc>')
          else
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infProt', '</infProt>');

          nProtLoc := RetornarConteudoEntre(XMLinfProt2.text, '<nProt>', '</nProt>');

          xProtNFe := '<protNFe versao="' + Versao + '">' +
                       '<infProt Id="ID'+ nProtLoc +'">'+
                        PreencherTAG('tpAmb', XMLinfProt.text) +
                        PreencherTAG('verAplic', XMLinfProt.text) +
                        PreencherTAG('chNFe', XMLinfProt.text) +
                        PreencherTAG('dhRecbto', XMLinfProt2.text) +
                        PreencherTAG('nProt', XMLinfProt2.text) +
                        PreencherTAG('digVal', XMLinfProt.text) +
                        PreencherTAG('cStat', XMLinfProt.text) +
                        PreencherTAG('xMotivo', XMLinfProt.text) +
                        PreencherTAG('cMsg', XMLinfProt.text) +
                        PreencherTAG('xMsg', XMLinfProt.text) +
                       '</infProt>' +
                      '</protNFe>';
        end;
      end;

      if ProtLido then
      begin
        (*By Edilson Alves de Oliveira  - 30/07/2015
          Resolver situação quando a UF utiliza servidor virtual *)
        if Copy(FverAplic,1,2) = 'SV' then
          xUF := CodigoUFparaUF(StrToIntDef(Copy(FchNFe, 1, 2), 0))
        else
          xUF := Copy(FverAplic,1,2);
        (**)
        xProtNFe := '<protNFe versao="' + Versao + '">' +
                     '<infProt Id="' + IfThen( Pos('ID', FnProt) > 0, FnProt, 'ID' + FnProt ) + '">' +
                      '<tpAmb>' + TpAmbToStr(FtpAmb) + '</tpAmb>' +
                      '<verAplic>' + FverAplic + '</verAplic>' +
                      '<chNFe>' + FchNFe + '</chNFe>' +
                      //'<dhRecbto>' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhRecbto) + '</dhRecbto>' +
                      //'<dhRecbto>' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhRecbto) + IIf(Versao >= '3.10', GetUTC{(copy(FverAplic,1,2),FdhRecbto)},'')+'</dhRecbto>'+
                      (*By Edilson Alves de Oliveira  - 30/07/2015 *)
                      '<dhRecbto>' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhRecbto) + IfThen(Versao >= '3.10', GetUTC(xUF,FdhRecbto),'')+'</dhRecbto>'+
                      '<nProt>' + FnProt + '</nProt>' +
                      '<digVal>' + FdigVal + '</digVal>' +
                      '<cStat>' + IntToStr(FcStat) + '</cStat>' +
                      '<xMotivo>' + FxMotivo + '</xMotivo>' +
                      '<cMsg>' + IntToStr(FcMsg) + '</cMsg>' +
                      '<xMsg>' + FxMsg + '</xMsg>' +
                     '</infProt>' +
                    '</protNFe>';
      end;

      FXML_NFe := XMLNFe.Text;
      FXML_prot := xProtNFe;
    end;

    // Gerar arquivo
    if (Gerador.ListaDeAlertas.Count = 0) and
       (FXML_NFe <> '') and (FXML_prot <> '') then
    begin
      Gerador.ArquivoFormatoXML := '';
      Gerador.wGrupo(ENCODING_UTF8, '', False);
      Gerador.wGrupo('nfeProc versao="' + Versao + '" ' + NAME_SPACE, '');
      Gerador.wTexto('<NFe xmlns' + RetornarConteudoEntre(FXML_NFe, '<NFe xmlns', '</NFe>') + '</NFe>');
      Gerador.wTexto(FXML_prot);
      Gerador.wGrupo('/nfeProc');
    end;

    Result := (Gerador.ListaDeAlertas.Count = 0);
  finally
    XMLNFE.Free;
    XMLinfProt.Free;
    XMLinfProt2.Free;
  end;
end;

procedure TProcNFe.Assign(Source: TProcNFe);
begin
//  Gerador.Assign(Source.Gerador);
  PathNFe            := Source.PathNFe;
  PathRetConsReciNFe := Source.PathRetConsReciNFe;
  PathRetConsSitNFe  := Source.PathRetConsSitNFe;
  tpAmb              := Source.tpAmb;
  verAplic           := Source.verAplic;
  chNFe              := Source.chNFe;
  dhRecbto           := Source.dhRecbto;
  nProt              := Source.nProt;
  digVal             := Source.digVal;
  cStat              := Source.cStat;
  xMotivo            := Source.xMotivo;
  Versao             := Source.Versao;
  cMsg               := Source.cMsg;
  xMsg               := Source.xMsg;

  XML_NFe            := Source.XML_NFe;
  XML_prot           := Source.XML_prot;
end;

end.

