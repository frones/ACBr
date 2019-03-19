////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar BPe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml do BPe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
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

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnProcBPe;

interface

uses
  SysUtils, Classes, pcnConversao, pcnGerador;

type

  TProcBPe = class(TPersistent)
  private
    FGerador: TGerador;
    FPathBPe: String;
    FPathRetConsReciBPe: String;
    FPathRetConsSitBPe: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FchBPe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
    FdigVal: String;
    FcStat: Integer;
    FxMotivo: String;
    FVersao: String;
    FcMsg: Integer;
    FxMsg: String;

    // Usando na Montagem do BPeProc
    FXML_BPe: String;
    FXML_prot: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GerarXML: Boolean;
  published
    property Gerador: TGerador          read FGerador;
    property PathBPe: String            read FPathBPe            write FPathBPe;
    property PathRetConsReciBPe: String read FPathRetConsReciBPe write FPathRetConsReciBPe;
    property PathRetConsSitBPe: String  read FPathRetConsSitBPe  write FPathRetConsSitBPe;
    property tpAmb: TpcnTipoAmbiente    read FtpAmb              write FtpAmb;
    property verAplic: String           read FverAplic           write FverAplic;
    property chBPe: String              read FchBPe              write FchBPe;
    property dhRecbto: TDateTime        read FdhRecbto           write FdhRecbto;
    property nProt: String              read FnProt              write FnProt;
    property digVal: String             read FdigVal             write FdigVal;
    property cStat: Integer             read FcStat              write FcStat;
    property xMotivo: String            read FxMotivo            write FxMotivo;
    property Versao: String             read FVersao             write FVersao;
    property cMsg: Integer              read FcMsg               write FcMsg;
    property xMsg: String               read FxMsg               write FxMsg;

    // Usando na Montagem do BPeProc
    property XML_BPe: String            read FXML_BPe            write FXML_BPe;
    property XML_prot: String           read FXML_prot           write FXML_prot;
  end;

implementation

uses
  pcnAuxiliar, pcnLeitor, ACBrUtil;

{ TProcBPe }

constructor TProcBPe.Create;
begin
  FGerador := TGerador.Create;
  FnProt   := '';
end;

destructor TProcBPe.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TProcBPe.GerarXML: Boolean;

  function PreencherTAG(const TAG: String; Texto: String): String;
  begin
    result := '<' + TAG + '>' + RetornarConteudoEntre(Texto, '<' + TAG + '>', '</' + TAG + '>') + '</' + TAG + '>';
  end;

var
  XMLBPe: TStringList;
  XMLinfProt: TStringList;
  XMLinfProt2: TStringList;
  wCstat: String;
  xProtBPe: String;
  nProtLoc: String;
  xUF: String;    
  LocLeitor: TLeitor;
  i: Integer;
  ProtLido: Boolean; //Protocolo lido do arquivo
begin
  XMLBPe      := TStringList.Create;
  XMLinfProt  := TStringList.Create;
  XMLinfProt2 := TStringList.Create;
  Gerador.ListaDeAlertas.Clear;

  try
    if (FXML_BPe = '') and (FXML_prot = '') then
    begin
      ProtLido := False;
      xProtBPe := '';

      // Arquivo BPe
      if not FileExists(FPathBPe) then
        Gerador.wAlerta('XR04', 'BPe', 'BPe', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
      else
        XMLBPe.LoadFromFile(FPathBPe);

      FchBPe := RetornarConteudoEntre(XMLBPe.Text, 'Id="BPe', '"');

      if trim(FchBPe) = '' then
        Gerador.wAlerta('XR01', 'ID/BPe', 'Numero da chave do BPe', ERR_MSG_VAZIO);

      if (FPathRetConsReciBPe = '') and (FPathRetConsSitBPe = '') then
      begin
        if (FchBPe = '') and (FnProt = '') then
          Gerador.wAlerta('XR06', 'RECIBO/SITUAÇÃO', 'RECIBO/SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
          ProtLido := True;
      end;

      // Gerar arquivo pelo Recibo da BPe                                       //
      if (FPathRetConsReciBPe <> '') and (FPathRetConsSitBPe = '') and (not ProtLido) then
      begin
        if not FileExists(FPathRetConsReciBPe) then
          Gerador.wAlerta('XR06', 'PROTOCOLO', 'PROTOCOLO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          I := 0;
          LocLeitor := TLeitor.Create;
          try
            LocLeitor.CarregarArquivo(FPathRetConsReciBPe);
            while LocLeitor.rExtrai(1, 'protBPe', '', i + 1) <> '' do
            begin
              if LocLeitor.rCampo(tcStr, 'chBPe') = FchBPe then
                FnProt := LocLeitor.rCampo(tcStr, 'nProt');

              if trim(FnProt) = '' then
                Gerador.wAlerta('XR01', 'PROTOCOLO/BPe', 'Numero do protocolo', ERR_MSG_VAZIO)
              else
              begin
                xProtBPe := LocLeitor.rExtrai(1, 'protBPe', '', i + 1); // +'</protBPe>';
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

      // Gerar arquivo pelo arquivo de consulta da situação do BPe              //
      if (FPathRetConsReciBPe = '') and (FPathRetConsSitBPe <> '') and (not ProtLido) then
      begin
        if not FileExists(FPathRetConsSitBPe) then
          Gerador.wAlerta('XR06', 'SITUAÇÃO', 'SITUAÇÃO', ERR_MSG_ARQUIVO_NAO_ENCONTRADO)
        else
        begin
          XMLinfProt.LoadFromFile(FPathRetConsSitBPe);

          wCstat := RetornarConteudoEntre(XMLinfProt.text, '<cStat>', '</cStat>');

          if ((trim(wCstat) = '101') or
              (trim(wCstat) = '151') or
              (trim(wCstat) = '155')) then //esta cancelado
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infCanc', '</infCanc>')
          else
            XMLinfProt2.Text := RetornarConteudoEntre(XMLinfProt.text, '<infProt', '</infProt>');

          nProtLoc := RetornarConteudoEntre(XMLinfProt2.text, '<nProt>', '</nProt>');

          xProtBPe := '<protBPe versao="' + Versao + '">' +
                       '<infProt Id="ID'+ nProtLoc +'">' +
                        PreencherTAG('tpAmb', XMLinfProt.text) +
                        PreencherTAG('verAplic', XMLinfProt.text) +
                        PreencherTAG('chBPe', XMLinfProt.text) +
                        PreencherTAG('dhRecbto', XMLinfProt2.text) +
                        PreencherTAG('nProt', XMLinfProt2.text) +
                        PreencherTAG('digVal', XMLinfProt.text) +
                        PreencherTAG('cStat', XMLinfProt.text) +
                        PreencherTAG('xMotivo', XMLinfProt.text) +
                       '</infProt>' +
                       '<infFisco>' +
                        PreencherTAG('cMsg', XMLinfProt.text) +
                        PreencherTAG('xMsg', XMLinfProt.text) +
                       '/<infFisco>' +
                      '</protBPe>';
        end;
      end;

      if ProtLido then
      begin
        if Copy(FverAplic, 1, 2) = 'SV' then
          xUF := CodigoParaUF(StrToIntDef(Copy(FchBPe, 1, 2), 0))
        else
          xUF := Copy(FverAplic, 1, 2);

        xProtBPe := '<protBPe versao="' + Versao + '">' +
                     '<infProt Id="' + IIf( Pos('ID', FnProt) > 0, FnProt, 'ID' + FnProt ) + '">' +
                      '<tpAmb>' + TpAmbToStr(FtpAmb) + '</tpAmb>' +
                      '<verAplic>' + FverAplic + '</verAplic>' +
                      '<chBPe>' + FchBPe + '</chBPe>' +
                      '<dhRecbto>' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FdhRecbto) +
                                     GetUTC(xUF, FdhRecbto) + '</dhRecbto>' +
                      '<nProt>' + FnProt + '</nProt>' +
                      '<digVal>' + FdigVal + '</digVal>' +
                      '<cStat>' + IntToStr(FcStat) + '</cStat>' +
                      '<xMotivo>' + FxMotivo + '</xMotivo>' +
                     '</infProt>' +
                     '<infFisco>' +
                      '<cMsg>' + IntToStr(FcMsg) + '</cMsg>' +
                      '<xMsg>' + FxMsg + '</xMsg>' +
                     '</infFisco>' +
                    '</protBPe>';
      end;

      FXML_BPe := XMLBPe.Text;
      FXML_prot := xProtBPe;
    end;

    // Gerar arquivo
    if (Gerador.ListaDeAlertas.Count = 0) and
       (FXML_BPe <> '') and (FXML_prot <> '') then
    begin
      Gerador.ArquivoFormatoXML := '';
      Gerador.wGrupo(ENCODING_UTF8, '', False);
      Gerador.wGrupo('BPeProc versao="' + Versao + '" ' + NAME_SPACE_BPE, '');
      Gerador.wTexto('<BPe xmlns' + RetornarConteudoEntre(FXML_BPe, '<BPe xmlns', '</BPe>') + '</BPe>');
      Gerador.wTexto(FXML_prot);
      Gerador.wGrupo('/BPeProc');
    end;

    Result := (Gerador.ListaDeAlertas.Count = 0);
  finally
    XMLBPe.Free;
    XMLinfProt.Free;
    XMLinfProt2.Free;
  end;
end;

procedure TProcBPe.Assign(Source: TPersistent);
begin
  if Source is TProcBPe then
  begin
    PathBPe := TprocBPe(Source).PathBPe;
    PathRetConsReciBPe := TprocBPe(Source).PathRetConsReciBPe;
    PathRetConsSitBPe := TprocBPe(Source).PathRetConsSitBPe;
    tpAmb := TprocBPe(Source).tpAmb;
    verAplic := TprocBPe(Source).verAplic;
    chBPe := TprocBPe(Source).chBPe;
    dhRecbto := TprocBPe(Source).dhRecbto;
    nProt := TprocBPe(Source).nProt;
    digVal := TprocBPe(Source).digVal;
    cStat := TprocBPe(Source).cStat;
    xMotivo := TprocBPe(Source).xMotivo;
    Versao := TprocBPe(Source).Versao;
    cMsg   := TprocBPe(Source).cMsg;
    xMsg   := TprocBPe(Source).xMsg;

    XML_BPe := TprocBPe(Source).XML_BPe;
    XML_prot := TprocBPe(Source).XML_prot;
  end
  else
    inherited;
end;

end.

