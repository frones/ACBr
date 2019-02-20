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

unit pcnRetAdmCSCNFCe;

interface

uses
  SysUtils, Classes, Contnrs, pcnConversao, pcnLeitor;

type

  TRetdadosCscCollectionItem = class;
  TRetAdmCSCNFCe             = class;

  TRetdadosCscCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetdadosCscCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetdadosCscCollectionItem);
  public
    function Add: TRetdadosCscCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetdadosCscCollectionItem;
    property Items[Index: Integer]: TRetdadosCscCollectionItem read GetItem write SetItem; default;
  end;

  TRetdadosCscCollectionItem = class(TObject)
  private
    FidCsc: Integer;
    FcodigoCsc: String;
  public
    property idCsc: Integer    read FidCsc     write FidCsc;
    property codigoCsc: String read FcodigoCsc write FcodigoCsc;
  end;

  TRetAdmCSCNFCe = class(TObject)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FindOP: TpcnIndOperacao;
    FcStat: Integer;
    FxMotivo: String;
    FdadosCsc: TRetdadosCscCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;

    property Leitor: TLeitor                  read FLeitor   write FLeitor;
    property versao: String                   read Fversao   write Fversao;
    property tpAmb: TpcnTipoAmbiente          read FtpAmb    write FtpAmb;
    property indOP: TpcnIndOperacao           read FindOP    write FindOP;
    property cStat: Integer                   read FcStat    write FcStat;
    property xMotivo: String                  read FxMotivo  write FxMotivo;
    property dadosCsc: TRetdadosCscCollection read FdadosCsc write FdadosCsc;
  end;

implementation

{ TRetdadosCscCollection }

function TRetdadosCscCollection.Add: TRetdadosCscCollectionItem;
begin
  Result := Self.New;
end;

function TRetdadosCscCollection.GetItem(
  Index: Integer): TRetdadosCscCollectionItem;
begin
  Result := TRetdadosCscCollectionItem(inherited GetItem(Index));
end;

procedure TRetdadosCscCollection.SetItem(Index: Integer;
  Value: TRetdadosCscCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TRetdadosCscCollection.New: TRetdadosCscCollectionItem;
begin
  Result := TRetdadosCscCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetAdmCSCNFCe }

constructor TRetAdmCSCNFCe.Create;
begin
  inherited Create;
  FLeitor   := TLeitor.Create;
  FdadosCsc := TRetdadosCscCollection.Create;
end;

destructor TRetAdmCSCNFCe.Destroy;
begin
  FLeitor.Free;
  FdadosCsc.Free;
  inherited;
end;

function TRetAdmCSCNFCe.LerXml: boolean;
var
  ok: boolean;
  i: Integer;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;
    if leitor.rExtrai(1, 'retAdmCscNFCe') <> '' then
    begin
      Fversao  := Leitor.rAtributo('versao');
      FtpAmb   := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FindOp   := StrToIndOperacao(Ok, Leitor.rCampo(tcStr, 'indOp'));
      FcStat   := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo := Leitor.rCampo(tcStr, 'xMotivo');

      i := 0;

      while Leitor.rExtrai(2, 'dadosCsc', '', i + 1) <> '' do
       begin
         FdadosCsc.Add;
         FdadosCsc.Items[i].FidCsc     := Leitor.rCampo(tcInt, 'idCsc');
         FdadosCsc.Items[i].FcodigoCsc := Leitor.rCampo(tcStr, 'codigoCsc');
         inc(i);
       end;

      if i = 0 then
      begin
        FdadosCsc.Add;
        FdadosCsc.Items[i].FidCsc     := 0;
        FdadosCsc.Items[i].FcodigoCsc := '';
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.

