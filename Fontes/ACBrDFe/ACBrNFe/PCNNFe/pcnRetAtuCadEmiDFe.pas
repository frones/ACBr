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
// Coordenação: Paulo Casagrande                                              //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU General Public License (GNU GPL)                          //
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

unit pcnRetAtuCadEmiDFe;

interface

uses
  SysUtils, Classes, pcnConversao, pcnLeitor;

type

  TretAtuCadEmiDFe = class(TPersistent)
  private
    FXML: TLeitor;
    FCNPJ: String;
    FresOpe: Integer;
    FUF: String;
    Fope: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property XML: TLeitor    read FXML    write FXML;
    property UF: String      read FUF     write FUF;
    property CNPJ: String    read FCNPJ   write FCNPJ;
    property ope: Integer    read Fope    write Fope;
    property resOpe: Integer read FresOpe write FresOpe;
  end;

implementation

{ TretAtuCadEmiDFe }

constructor TretAtuCadEmiDFe.Create;
begin
  FXML := TLeitor.Create;
end;

destructor TretAtuCadEmiDFe.Destroy;
begin
  FXML.Free;
  inherited;
end;

function TretAtuCadEmiDFe.LerXml: Boolean;
begin
  result := true;
  try
    (*N05*)FUF     := xml.rCampo(tcStr, 'UF');
    (*N06*)FCNPJ   := xml.rCampo(tcStr, 'CNPJ');
    (*N07*)Fope    := xml.rCampo(tcInt, 'ope');
    (*N08*)FresOpe := xml.rCampo(tcInt, 'resOpe');
  except
    result := false;
  end;
end;

end.

