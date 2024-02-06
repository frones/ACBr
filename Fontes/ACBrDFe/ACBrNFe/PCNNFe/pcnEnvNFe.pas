////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
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

{$I ACBr.inc}

unit pcnEnvNFe;

interface

uses
  SysUtils, Classes,
  ACBrDFeConsts,
  pcnConversao, pcnGerador, pcnNFeConsts;

type

  TenvNFe = class(TPersistent)
  private
    FGerador: TGerador;
    FidLote: String;
    FListaDeArquivos: TStringlist;
    FVersao: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
    function ObterNomeArquivo: String;
    function AdicionarArquivo(Path: String): Boolean;
  published
    property Gerador: TGerador            read FGerador         write FGerador;
    property idLote: String               read FidLote          write FidLote;
    property ListaDeArquivos: TStringList read FListaDeArquivos write FListaDeArquivos;
    property Versao: String               read FVersao          write FVersao;
  end;

implementation

Uses pcnAuxiliar;

{ TenvNFe }

constructor TenvNFe.Create;
begin
  FGerador         := TGerador.Create;
  FListaDeArquivos := TStringList.Create;
end;

destructor TenvNFe.Destroy;
begin
  FGerador.Free;
  FListaDeArquivos.Free;
  inherited;
end;

function TenvNFe.ObterNomeArquivo: String;
begin
  Result := FidLote + '-env-lot.xml';
end;

function TenvNFe.AdicionarArquivo(Path: String): Boolean;
begin
  Result := False;
  if FListaDeArquivos.Count = 50 then
    exit;
  if not FileExists(Path) then
    exit;
  FListaDeArquivos.add(Path);
  Result := True;
end;

function TenvNFe.GerarXML: Boolean;
var
  i: Integer;
  XMLNFE: TStringList;
begin
  Result := False;

  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo(ENCODING_UTF8_STD, '', False);
  Gerador.wGrupo('enviNFe ' + NAME_SPACE + ' versao="' + Versao + '"');
  Gerador.wCampo(tcStr, 'AP03', 'idLote', 001, 015, 1, FIdLote, DSC_IdLote);
  for i := 0 to FlistaDeArquivos.count - 1 do
  begin
    XMLNFE := TStringList.create;
    if not FileExists(FListaDeArquivos[i]) then
    begin
      Gerador.wAlerta('AP04', 'NFE', 'NFE', ERR_MSG_ARQUIVO_NAO_ENCONTRADO);
    end
    else
      XMLNFE.LoadFromFile(FListaDeArquivos[i]);
    Gerador.wTexto('<NFe xmlns' + RetornarConteudoEntre(XMLNFE.Text, '<NFe xmlns', '</NFe>') + '</NFe>');
    XMLNFE.Free;
  end;
  Gerador.wGrupo('/enviNFe');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

