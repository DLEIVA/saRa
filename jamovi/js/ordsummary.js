// Supplier items include public column metadata, including selected (used) items.
const numericSelections = new WeakMap();
const events = {
    update: function(ui) {
        const selected = ui.vars.value() || [];
        const items = ui.ordinalSupplier.value() || [];
        const ordinalItems = items.filter(item => item.properties.measureType === 'ordinal');
        if (ordinalItems.length !== items.length) ui.ordinalSupplier.setValue(ordinalItems);
        const hasNumeric = ordinalItems.some(item => selected.includes(item.value.raw) &&
            item.properties.measureType === 'ordinal' &&
            ['integer', 'decimal'].includes(item.properties.dataType));
        const completeMetadata = selected.length > 0 && selected.every(value =>
            ordinalItems.some(item => item.value.raw === value));
        if (completeMetadata) {
            const previous = numericSelections.get(ui);
            if (!hasNumeric && ui.range.value()) ui.range.setValue(false);
            else if (hasNumeric && previous === false) ui.range.setValue(true);
            numericSelections.set(ui, hasNumeric);
        }
        const numericOnly = ['trimean', 'avquartile', 'midrange', 'range', 'iqr',
            'qd', 'mad', 'rcv', 'irange', 'h1', 'h3', 'k2', 'k3', 'box', 'violin', 'dot'];
        for (const name of numericOnly) {
            if (ui[name]) ui[name].setPropertyValue('enable', hasNumeric);
        }
        if (ui.NPerc) ui.NPerc.setPropertyValue('enable', hasNumeric && ui.irange.value());
        if (ui.dotType) ui.dotType.setPropertyValue('enable', hasNumeric && ui.dot.value());
        for (const name of ['innerf', 'outerf'])
            if (ui[name]) ui[name].setPropertyValue('enable', hasNumeric && (ui.box.value() || ui.dot.value()));
    }
};
module.exports = {
    view_loaded: events.update,
    view_updated: events.update,
    ordinalSupplier_changed: events.update,
    vars_changed: events.update
};
