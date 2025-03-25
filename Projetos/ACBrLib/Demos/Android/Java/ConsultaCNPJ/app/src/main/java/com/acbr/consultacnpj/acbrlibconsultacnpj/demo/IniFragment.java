package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import androidx.fragment.app.Fragment;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;

public class IniFragment extends Fragment {
    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;
    private TextInputEditText txtIni;
    private TextInputLayout layoutIni;
    private Button btnRevelarIni;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_ini, container, false);

        ACBrConsultaCNPJ = ACBrLibHelper.getInstance("");

        txtIni = view.findViewById(R.id.txtIni);
        layoutIni = view.findViewById(R.id.layoutIni);
        btnRevelarIni = view.findViewById(R.id.btnRevelarIni);

        btnRevelarIni.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (layoutIni.getVisibility() == View.GONE) {
                    lerIni();
                    layoutIni.setVisibility(View.VISIBLE);
                    btnRevelarIni.setText("Ocultar conteúdo do INI");
                    btnRevelarIni.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_eye_off, 0, 0, 0);
                } else {
                    layoutIni.setVisibility(View.GONE);
                    btnRevelarIni.setText("Mostrar conteúdo do INI");
                    btnRevelarIni.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_eye, 0, 0, 0);
                }
            }
        });

        return view;
    }

    private void lerIni() {
        try {
            String ini = ACBrConsultaCNPJ.configExportar();
            txtIni.setText(ini);
        } catch (Exception ex) {
            Log.e("Erro", " - Ler INI: " + ex.getMessage());
        }
    }
} 