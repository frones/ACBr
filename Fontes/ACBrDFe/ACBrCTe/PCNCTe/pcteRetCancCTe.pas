////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar CTe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da CTe          //
//                                                                            //
//        site: www.projetocooperar.org/cte                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_cte/        //
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
//              "PCN  -  Projeto  Cooperar  CTe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcteRetCancCTe;

interface

uses
  SysUtils, Classes, pcnAuxiliar, pcnConversao, pcnLeitor;

type

  TRetCancCTe = class(TPersistent)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FId: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FchCTe: String;
    FdhRecbto: TDateTime;
    FnProt: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
  published
    property Leitor: TLeitor         read FLeitor   write FLeitor;
    property versao: String          read Fversao   write Fversao;
    property Id: String              read FId       write FId;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property verAplic: String        read FverAplic write FverAplic;
    property cStat: Integer          read FcStat    write FcStat;
    property xMotivo: String         read FxMotivo  write FxMotivo;
    property cUF: Integer            read FcUF      write FcUF;
    property chCTe: String           read FchCTe    write FchCTe;
    property dhRecbto: TDateTime     read FdhRecbto write FdhRecbto;
    property nProt: String           read FnProt    write FnProt;
  end;

implementation

{ TRetCancCTe }

constructor TRetCancCTe.Create;
begin
  FLeitor := TLeitor.Create;
end;

destructor TRetCancCTe.Destroy;
begin
  FLeitor.Free;
  inherited;
end;

function TRetCancCTe.LerXml: boolean;
var
  ok: boolean;
begin
  Result := False;
  try
    Fversao := Leitor.rAtributo('versao', 'retCancCTe');

    if Leitor.rExtrai(1, 'infCanc') <> '' then
    begin
      (*CR04 *)FId       := Leitor.rAtributo('Id=', 'infCanc');
      (*CR05 *)FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      (*CR06 *)FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      (*CR07 *)FcStat    := Leitor.rCampo(tcInt, 'cStat');
      (*CR08 *)FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      (*CR08a*)FcUF      := Leitor.rCampo(tcInt, 'cUF');

      if cStat = 101 then
      begin
        (*CR09 *)FchCTe    := Leitor.rCampo(tcStr, 'chCTe');
        (*CR10 *)FdhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
        (*CR11 *)FnProt    := Leitor.rCampo(tcStr, 'nProt');
      end;
      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.

