package com.acbr.nfe.acbrlibnfe.demo.utils;

import android.content.Context;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Spinner;

public class SpinnerUtils {

    /**
     * Preenche um Spinner com os itens fornecidos e define um listener para seleção de itens.
     *
     * @param context O contexto atual.
     * @param spinner O Spinner a ser preenchido.
     * @param items   Os itens a serem exibidos no Spinner.
     * @param <T>     O tipo dos itens.
     */
    public static <T> void preencherSpinner(Context context, Spinner spinner, T[] items) {
        ArrayAdapter<T> adapter = new ArrayAdapter<>(
                context,
                android.R.layout.simple_spinner_item,
                items
        );
        adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        spinner.setAdapter(adapter);

        spinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                T selectedItem = (T) parent.getItemAtPosition(position);
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });
    }
}
